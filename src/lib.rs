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

pub trait HashCache<'l, T>
where
    T: Hash,
{
    fn hash32(segment: &T) -> u32;
    fn segment_change(&mut self, index: usize, tail: &'l [T]) -> Option<Change<&'l T>>;
}

// TODO implement `PartialOrd` interns of index
#[derive(Copy, Clone, Debug, PartialEq)]
pub struct IndexMap<'l, T> {
    index: usize,
    segment: &'l T,
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub enum IndexState<'l, T> {
    /// Cached holds the old `IndexMap`
    Cached(IndexMap<'l, T>),
    /// `Found` holds the new `IndexMap`
    Found(IndexMap<'l, T>),
}
use self::IndexState::*;

impl<'l, T> IndexState<'l, T> {
    fn is_cached(&self) -> bool {
        match self {
            Cached(_) => true,
            _ => false,
        }
    }

    fn cached(&self) -> Option<IndexMap<'l, T>> {
        match self {
            Cached(IndexMap { index, segment }) => Some(IndexMap {
                index: *index,
                segment,
            }),
            _ => None,
        }
    }
}

type Flag = bool;

#[derive(Clone, Debug, PartialEq)]
pub struct DefaultHashCache<'l, T> {
    /// `(flag,  IndexMap<'l, T>) => Cached(IndexMap<'l, T>)` used to view `cache`
    flag: Flag,
    /// Map from hash to `IndexState` where `Flag` is interpreted with `flag`
    cache: IntMap<u32, SmallVec<[(Flag, IndexMap<'l, T>); 2]>>,
}

impl<'l, T> DefaultHashCache<'l, T> {
    fn view_state(flag: Flag, raw: &(Flag, IndexMap<'l, T>)) -> IndexState<'l, T> {
        let (raw_flag, IndexMap { index, segment }) = raw;
        if flag == *raw_flag {
            Cached(IndexMap {
                index: *index,
                segment: *segment,
            })
        } else {
            Found(IndexMap {
                index: *index,
                segment: *segment,
            })
        }
    }

    fn set_state(flag: Flag, raw: &mut (Flag, IndexMap<'l, T>), to: IndexState<'l, T>) {
        let (raw_flag, raw_index_map) = raw;

        match to {
            Cached(index_map) => {
                *raw_flag = flag;
                *raw_index_map = index_map;
            }
            Found(index_map) => {
                *raw_flag = !flag;
                *raw_index_map = index_map;
            }
        }
    }

    fn toggle(&mut self) {
        self.flag = !self.flag
    }
}

impl<'l, T> HashCache<'l, T> for DefaultHashCache<'l, T>
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

    fn segment_change(&mut self, new_index: usize, new_tail: &'l [T]) -> Option<Change<&'l T>> {
        // caller must ensure new_tail holds at least one
        let new_segment = &new_tail[0];
        let hash = DefaultHashCache::hash32(new_segment);
        let Self { flag, cache } = self;
        let mut change = None;
        let unsafe_cache =
            unsafe { &*{ cache as *const IntMap<u32, SmallVec<[(Flag, IndexMap<'l, T>); 2]>> } };

        cache.entry(hash).and_modify(|index_maps| {
            // index_map is contains refs to any Index content pairs with a equal hash
            // We handle hash collisions by checking segment content

            // This is safe because we delete the entry if the `SmallVec` becomes empty.
            // TODO this should be proven with an `AtLeastOne` constructor type

            {
                let cached_im = index_maps
                    .iter()
                    .map(|relation| DefaultHashCache::view_state(*flag, relation))
                    .filter(|state| state.is_cached())
                    .map(|cached| cached.cached().unwrap());

                let no_change = cached_im
                    .filter(|IndexMap { index, segment }| {
                        *index == new_index && **segment == new_tail[0]
                    })
                    .next()
                    .is_some();

                if no_change {
                    // return true
                    unimplemented!()
                }
            }

            {
                // FIXME This duplicate should be a higher order function.
                // Blocked on type system
                let cached_im = index_maps
                    .iter_mut()
                    .map(|relation| DefaultHashCache::view_state(*flag, relation))
                    .filter(|state| state.is_cached())
                    .map(|cached| cached.cached().unwrap());

                let eq_segment = cached_im
                    .filter(|IndexMap { segment, .. }| **segment == new_tail[0])
                    .next();

                if let Some(IndexMap { index, segment }) = eq_segment {
                    // We have either a deletion, or an addition.

                    // index >= new_index
                    // due to deleted old entries being removed from map
                    // FIXME remove deleted entries
                    let deletion_len = index - new_index;
                    let expect_index = index;

                    // For Change to be a deletion:
                    // tail[N <- 0..deletion_len].index == old[index+N].index

                    let is_deletion = new_tail
                        .iter()
                        .skip(1)
                        .map(|new_segment| {
                            unsafe_cache
                                .get(&DefaultHashCache::hash32(new_segment))
                                .map_or(false, |index_map| {
                                    index_map
                                        .iter()
                                        .map(|i| {
                                            if let Cached(IndexMap { index, segment }) =
                                                Self::view_state(*flag, i)
                                            {
                                                index == expect_index && segment == new_segment
                                            } else {
                                                false
                                            }
                                        })
                                        .any(|b| b)
                                })
                        })
                        .enumerate()
                        .take_while(|(i, _)| *i != deletion_len)
                        .all(|(_, b)| b);
                }
            }
            let no_change = index_maps
                .iter_mut()
                // FIXME this map logic is wrong
                // find equal index then check for equal segments
                // if that fails check for equal segments
                .map(|i| {
                    if let Cached(IndexMap { index, segment }) = Self::view_state(*flag, i) {
                        // TODO Segment comparison is far more expensive,
                        // so this should be a nested if statement since match is eager.
                        match (segment == new_segment, index == new_index) {
                            (true, true) => {
                                // No Change
                                Self::set_state(
                                    *flag,
                                    i,
                                    Found(IndexMap {
                                        index: new_index,
                                        segment: new_segment,
                                    }),
                                );
                                true
                            }
                            (true, false) => unimplemented!(),
                            (..) => false,
                        }
                    } else {
                        false
                    }
                })
                .find(|b| *b);
            change = Some(Change::Added(new_segment));
        });
        change
    }
}

impl<'l, T> From<&'l [T]> for DefaultHashCache<'l, T>
where
    T: Hash + PartialEq,
{
    fn from(slice: &'l [T]) -> DefaultHashCache<'l, T> {
        let flag = false;
        let cache = slice
            .iter()
            .enumerate()
            .map(|(index, segment)| {
                (
                    // key
                    DefaultHashCache::hash32(segment),
                    // value
                    smallvec![(flag, IndexMap { index, segment })],
                )
            })
            .collect();

        DefaultHashCache { flag, cache }
    }
}

pub trait Diffable<'l, T>
where
    T: Hash,
{
    fn hash_diff(self, new: Self) -> Diff<'l, T, DefaultHashCache<'l, T>>;
    /// Utilizes Cache of old hashes to speedup diff. Returns Diff and the `HashCache` of new.
    /// If you will be differencing more than 1 new slice against old,
    /// you should clone the cache before passing it in.
    fn hash_diff_cached<C>(self, old_cache: C, new: Self) -> Diff<'l, T, C>
    where
        C: HashCache<'l, T> + From<&'l [T]>;
}

#[derive(Copy, Clone, Debug, PartialEq)]
pub struct Diff<'l, T, C> {
    future: Option<Change<&'l T>>,
    changed_new: &'l [T],
    cache: C,
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
    fn hash_diff(self, new: Self) -> Diff<'l, T, DefaultHashCache<'l, T>> {
        // Properly implement so that a cache can not be extracted
        self.hash_diff_cached(DefaultHashCache::from(self), new)
    }

    fn hash_diff_cached<C>(self, cache: C, new: Self) -> Diff<'l, T, C>
    where
        C: HashCache<'l, T>,
    {
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

impl<'l, T, C> Iterator for Diff<'l, T, C>
where
    C: HashCache<'l, T>,
    T: Hash,
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
                        let keep_going = cache.segment_change(index, &changed_new[index..]).map(
                            |future_change| {
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
                            },
                        );

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
