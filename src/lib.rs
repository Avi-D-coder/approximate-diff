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
    fn segment_change(&mut self, index: usize, segment: &'l T) -> Option<Change<'l, T>>;
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

type Flag = bool;

#[derive(Clone, Debug)]
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

    fn segment_change(&mut self, new_index: usize, new_segment: &'l T) -> Option<Change<'l, T>> {
        let hash = DefaultHashCache::hash32(new_segment);
        let Self { flag, cache } = self;
        cache.entry(hash).and_modify(|index_map| {
            // This is safe because we delete the entry if the `SmallVec` becomes empty.
            // TODO this should be proven with an `AtLeastOne` constructor type
            for i in index_map.iter_mut() {
                if let Cached(IndexMap { index, segment }) = Self::view_state(*flag, i) {
                    if segment == new_segment {
                        Self::set_state(
                            *flag,
                            i,
                            Found(IndexMap {
                                index: new_index,
                                segment: new_segment,
                            }),
                        );
                    }
                }
            }
        });
        unimplemented!()
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

pub trait Diffable<'l, I>
where
    I: Hash,
{
    fn hash_diff(self, new: Self) -> Diff<'l, I>;
    /// Utilizes Cache of old hashes to speedup diff. Returns Diff and the `HashCache` of new.
    /// If you will be differencing more than 1 new slice against old,
    /// you should clone the cache before passing it in.
    fn hash_diff_cached<C>(self, old_cache: C, new: Self) -> (Diff<'l, I>, C)
    where
        C: HashCache<'l, I> + From<&'l [I]>;
}

pub struct Diff<'l, I> {
    old: &'l [I],
    new: &'l [I],
}

pub enum Change<'l, I> {
    Added(&'l [I]),
    Removed(&'l [I]),
}

impl<'l, I> Diffable<'l, I> for &'l [I]
where
    I: PartialEq + Hash,
{
    fn hash_diff(self, new: Self) -> Diff<'l, I> {
        self.hash_diff_cached(DefaultHashCache::from(self), new).0
    }

    fn hash_diff_cached<C>(self, cache: C, new: Self) -> (Diff<'l, I>, C)
    where
        C: HashCache<'l, I>,
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
            return unimplemented!();
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

        let changed_old = &old[diff_start..min(old.len(), diff_end)];
        let changed_new = &new[diff_start..min(new.len(), diff_end)];
        unimplemented!()
    }
}

impl<'l, I> Iterator for Diff<'l, I> {
    type Item = Change<'l, I>;
    fn next(&mut self) -> Option<Change<'l, I>> {
        unimplemented!()
    }
}

#[test]
fn vec_diff() {
    let o = vec![1, 2, 3, 4, 5];
    let n = vec![1, 2, 3, 4, 5];

    o.as_slice().hash_diff(n.as_slice());
}
