mod predicate;

use std::cmp::{max, min};
use std::collections::HashMap;
use std::hash::{BuildHasher, Hash, Hasher};

extern crate itertools;
use itertools::EitherOrBoth::{Both, Right};
use itertools::Itertools;

extern crate nohash_hasher;
use nohash_hasher::IntMap;

extern crate fasthash;
use fasthash::murmur3::Murmur3Hasher_x86_32;

extern crate smallvec;
use smallvec::{smallvec, SmallVec};

type DiffIndex = usize;

// TODO implement `PartialOrd` interns of index
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct IndexMap<'l, T> {
    index: DiffIndex,
    segment: &'l T,
}

#[derive(Clone, Debug, PartialEq)]
pub struct HashCache<'l, T> {
    cache: IntMap<u32, SmallVec<[IndexMap<'l, T>; 2]>>,
}

impl<'l, T> HashCache<'l, T>
where
    T: Hash + PartialEq,
{
    fn hash32(segment: &T) -> u32 {
        // TODO I think this allocates unnecessarily
        // If so it needs to be fixed in th rust-fasthash crate
        let mut hasher = Murmur3Hasher_x86_32::default();
        segment.hash(&mut hasher);
        hasher.finish() as u32
    }

    fn segment_change(
        &mut self,
        new_index: DiffIndex,
        new_tail: &'l [T],
        // TODO implement macro over sizes
        pred_changes: SmallVec<[predicate::Change; 10]>,
    ) -> Option<Change<&'l [T]>> {
        // caller must ensure new_tail holds at least one
        let mut ret = None;
        let new_segment = &new_tail[0];
        let hash = HashCache::hash32(new_segment);
        let Self { cache } = self;
        let unsafe_cache =
            unsafe { &*{ cache as *const IntMap<u32, SmallVec<[IndexMap<'l, T>; 2]>> } };

        cache.entry(hash).and_modify(|index_maps| {
            // index_map is contains refs to any Index content pairs with a equal hash
            // We handle hash collisions by checking segment content

            // This is safe because we delete the entry if the `SmallVec` becomes empty.
            // TODO this should be proven with an `AtLeastOne` constructor type

            {
                let cached_im = index_maps
                    .iter()
                    .filter(|IndexMap { index, .. }| *index >= new_index);

                let no_change = cached_im
                    .filter(|IndexMap { index, segment }| {
                        *index == new_index && **segment == new_tail[0]
                    })
                    .next()
                    .is_some();

                if no_change {
                    // return true
                    // ret = None
                    return;
                }
            }

            {
                // FIXME This duplicate should be a higher order function.
                // Blocked on type system
                let cached_im = index_maps
                    .iter_mut()
                    .filter(|IndexMap { index, .. }| *index >= new_index);

                let eq_segment = cached_im
                    .filter(|IndexMap { segment, .. }| **segment == new_tail[0])
                    .next();

                if let Some(IndexMap { index, segment }) = eq_segment {
                    // We have either a deletion, or an addition.

                    // index >= new_index
                    // due to deleted old entries being removed from map
                    // FIXME remove deleted entries
                    let deletion_len = *index - new_index;
                    let deletion_expect_index = *index;

                    // For Change to be a deletion:
                    // tail[N <- 0..deletion_len].index == old[index+N].index

                    let mut deletion_amt = 0;
                    let is_deletion =
                        new_tail
                            .iter()
                            .skip(1)
                            .enumerate()
                            .scan(deletion_len, |deletion_len, (n, new_segment)| {
                                unsafe_cache.get(&HashCache::hash32(new_segment)).map_or(
                                    Some((*deletion_len, false)),
                                    |index_map| {
                                        // is consistent with being a
                                        let (addition, deletion) = index_map
                                        .iter()
                                        // Our diff algorithm never backtracks,
                                        // so >= states unfound segment.
                                        .filter(|IndexMap { index, .. }| *index >= new_index)
                                        .scan(
                                            (false, false),
                                            |(addition, deletion), IndexMap { index, segment }| {
                                                // TODO check asm to make sure LLVM makes this lazy
                                                let eq_seg = *segment == new_segment;

                                                if *index == new_index + n && eq_seg {
                                                    // TODO cache additions
                                                    // This would be a no change

                                                    *addition = true;
                                                    Some((*addition, *deletion))
                                                } else if *index == deletion_expect_index + n
                                                    && eq_seg
                                                {
                                                    *deletion = true;
                                                    Some((*addition, *deletion))
                                                } else {
                                                    Some((*addition, *deletion))
                                                }
                                            },
                                        ).take_while(|t| *t != (true, true) ).last()
                                        // This is safe due to the requirement that
                                        // for each entry hash -> SmallVec.len >= 1
                                        .unwrap();

                                        if !deletion {
                                            Some((*deletion_len, false))
                                        } else if !addition {
                                            *deletion_len -= 2;
                                            Some((*deletion_len, true))
                                        } else {
                                            *deletion_len -= 1;
                                            Some((*deletion_len, true))
                                        }
                                    },
                                )
                            })
                            .take_while(|(deletion_len, _)| {
                                deletion_amt = *deletion_len;
                                *deletion_len != 0
                            })
                            .all(|(_, b)| b);

                    if is_deletion {
                        deletion_amt += new_index;

                        // return deleted
                        ret = Some(Change::Removed(&new_tail[0..deletion_amt]));
                        return;
                    } else {
                        // return addition
                        ret = Some(Change::Added(&new_tail[0..1]));
                        // FIXME handle pending deletion this gives rise to
                        return;
                    }
                }
            }
        });
        ret
    }
}

impl<'l, T> From<&'l [T]> for HashCache<'l, T>
where
    T: Hash + PartialEq,
{
    fn from(slice: &'l [T]) -> HashCache<'l, T> {
        let flag = false;
        let cache = slice
            .iter()
            .enumerate()
            .map(|(index, segment)| {
                (
                    // key
                    HashCache::hash32(segment),
                    // value
                    smallvec![IndexMap { index, segment }],
                )
            })
            .collect();

        HashCache { cache }
    }
}

pub trait Diffable<'l, T>
where
    T: Hash,
{
    fn hash_diff(self, new: Self) -> Diff<'l, T>;
    /// Utilizes Cache of old hashes to speedup diff. Returns Diff and the `HashCache` of new.
    /// If you will be differencing more than 1 new slice against old,
    /// you should clone the cache before passing it in.
    fn hash_diff_cached(self, old_cache: HashCache<'l, T>, new: Self) -> Diff<'l, T>;
}

#[derive(Clone, Debug, PartialEq)]
pub struct Diff<'l, T> {
    future: Option<Change<&'l T>>,
    changed_new: &'l [T],
    cache: HashCache<'l, T>,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum Change<S> {
    Added(S),
    Removed(S),
}

/// ChangeVariant is used for a `PartialEq` of variants.
/// Such that `==` does not care about the contents of the change.
#[derive(Copy, Clone, Debug, PartialEq)]
enum ChangeVariant {
    Added,
    Removed,
    // FIXME handle moved lines
}

impl<S> From<Change<S>> for ChangeVariant {
    fn from(change: Change<S>) -> ChangeVariant {
        match change {
            Change::Added(_) => ChangeVariant::Added,
            Change::Removed(_) => ChangeVariant::Removed,
        }
    }
}

impl<S> From<(ChangeVariant, S)> for Change<S> {
    fn from(from: (ChangeVariant, S)) -> Change<S> {
        match from {
            (ChangeVariant::Removed, segment) => Change::Removed(segment),
            (ChangeVariant::Added, segment) => Change::Added(segment),
        }
    }
}

impl<'l, T> Diffable<'l, T> for &'l [T]
where
    T: PartialEq + Hash,
{
    fn hash_diff(self, new: Self) -> Diff<'l, T> {
        // Properly implement so that a cache can not be extracted
        self.hash_diff_cached(HashCache::from(self), new)
    }

    fn hash_diff_cached(self, cache: HashCache<'l, T>, new: Self) -> Diff<'l, T> {
        let old = self;

        let longer = max(old.len(), new.len());
        let diff_start = old
            .iter()
            .zip(new.iter())
            .enumerate()
            .find(|(_, (o, n))| o != n)
            .map_or(0, |(l, _)| l);

        if diff_start == longer {
            // Here zip proved both new and old have same length.
            // So there are no changes.
            return Diff {
                future: None,
                changed_new: &[],
                cache,
            };
        }

        let diff_end = old
            .iter()
            .zip_longest(new.iter())
            .enumerate()
            .rfind(|(_, seg)| match seg {
                Both(o, n) => o != n,
                _ => true,
            })
            .map_or(0, |(l, _)| l + 1);

        let changed_new = &new[diff_start..min(new.len(), diff_end)];
        Diff {
            future: None,
            changed_new,
            cache,
        }
    }
}

impl<'l, T> Iterator for Diff<'l, T>
where
    T: Hash + PartialEq,
{
    type Item = Change<&'l [T]>;
    fn next(&mut self) -> Option<Change<&'l [T]>> {
        let Diff {
            changed_new,
            future,
            cache,
        } = self;

        match (changed_new.is_empty(), future) {
            (false, None) => {
                changed_new.iter().enumerate().scan(
                    None,
                    // FIXME this is a for loop
                    |next_change: &mut Option<Change<&'l [T]>>, (index, _)| {
                        let keep_going = cache
                            .segment_change(index, &changed_new[index..], SmallVec::new())
                            .map(|future_change| {
                                let future_variant = ChangeVariant::from(future_change);
                                let next_variant = next_change
                                    .map(|nc| ChangeVariant::from(nc))
                                    .unwrap_or(future_variant);

                                if next_variant == future_variant {
                                    let change =
                                        Change::from((next_variant, &changed_new[0..index]));
                                    *next_change = Some(change);
                                    (Some(change), None)
                                } else {
                                    (next_change.clone(), Some(future_change.clone()))
                                }
                            });

                        if keep_going.is_none() && next_change.is_none() {
                            Some((None, None))
                        } else {
                            keep_going
                        }
                    },
                );

                unimplemented!()
            }
            (true, Some(future_item)) => unimplemented!(),
            _ => None,
        }
    }
}

#[test]
fn vec_diff() {
    let o = vec![1, 2, 3, 4, 5];
    let n = vec![1, 2, 3, 4, 5];

    o.as_slice().hash_diff(n.as_slice());
}
