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

pub trait HashCache<T>
where
    T: Hash,
{
    fn hash_at(&mut self, index: usize, content: &T) -> IndexToIndex;
}

#[derive(Clone, Debug)]
enum Complete {
    Complete,
    Incomplete,
}

impl Default for Complete {
    fn default() -> Self {
        Complete::Incomplete
    }
}

type Index = usize;

#[derive(Clone, Debug)]
pub enum IndexToIndex {
    Cached(Index),
    Found(Index, Index),
}

#[derive(Clone, Debug)]
pub enum Position {
    Zero,
    First,
}

impl Default for Position {
    fn default() -> Self {
        // old starts on the left
        Position::First
    }
}

#[derive(Clone, Default, Debug)]
pub struct DefaultHashCache {
    status: Complete,
    new_position: Position,
    /// Map from hash to indexes
    cache: IntMap<u32, SmallVec<[IndexToIndex; 2]>>,
}

impl<T> HashCache<T> for DefaultHashCache
where
    T: Hash,
{
    fn hash_at(&mut self, index: usize, content: &T) -> IndexToIndex {
        unimplemented!()
        // *self.cache.entry(index).or_insert_with(|| {
        //     // FIXME this is ugly
        //     let mut hasher = Murmur3Hasher_x86_32::default();
        //     content.hash(&mut hasher);
        //     hasher.finish() as u32
        // })
    }
}

impl<'l, T> From<&'l [T]> for DefaultHashCache
where
    T: Hash,
{
    fn from(slice: &'l [T]) -> DefaultHashCache {
        let cache = slice
            .iter()
            .enumerate()
            .map(|(index, content)| {
                let mut hasher = Murmur3Hasher_x86_32::default();
                content.hash(&mut hasher);
                let hash = hasher.finish() as u32;

                (hash, smallvec![IndexToIndex::Cached(index)])
            })
            .collect();

        DefaultHashCache {
            status: Complete::Complete,
            new_position: Position::default(),
            cache,
        }
    }
}

pub trait Diffable<'l, I>
where
    I: Hash,
{
    fn hash_diff(self, new: Self) -> Diff<'l, I>;
    /// Utilizes Cache of old hashes to speedup diff. Returns Diff and the `HashCache` of new.
    /// If you will be diffing more than 1 new slice against old,
    /// you should clone the cache before passing it in.
    fn hash_diff_cached<C>(self, old_cache: C, new: Self) -> (Diff<'l, I>, C)
    where
        C: HashCache<I> + From<&'l [I]>;
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
        self.hash_diff_cached(DefaultHashCache::default(), new).0
    }

    fn hash_diff_cached<C>(self, cache: C, new: Self) -> (Diff<'l, I>, C)
    where
        C: HashCache<I>,
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
