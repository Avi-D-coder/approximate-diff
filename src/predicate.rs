use smallvec::{Array, SmallVec};
use std::ops::{Index, Range};

// TODO Bounded version
pub struct Tree<A>
where
    A: smallvec::Array,
{
    vec: SmallVec<A>,
}

impl<A> Tree<A>
where
    A: Array<Item = Change>,
{
    fn new(arr: A) -> Tree<A> {
        Tree {
            // TODO IntMap alternative
            vec: SmallVec::from_buf(arr),
        }
    }

    /// panics on invalid index passed to `at`
    fn remove(mut self, at: usize) {
        if at == self.vec.len() - 1 {
            return;
        }

        let len = self.vec.len();
        let mut i = at;
        let mut next = at + 1;

        let mut removed: SmallVec<[usize; 128]> = SmallVec::from_elem(at, 1);
        while i < len {
            if next < len {
                let next_apon: Option<usize> = self.vec[next].apon.into();
                let remove_next: bool = next_apon.map_or(false, |index| removed.contains(&index));

                if remove_next {
                    removed.push(next);
                    next += 1;
                } else {
                    self.vec[i] = self.vec[next].clone();
                    i += 1;
                    next += 1;
                }
            } else {
                self.vec.truncate(i + 1);
                break;
            }
        }
    }
}

impl<U, A> Index<usize> for Tree<A>
where
    A: Array<Item = U>,
{
    type Output = U;

    fn index(&self, index: usize) -> &Self::Output {
        &self.vec[index]
    }
}

impl<U, A> Index<Range<usize>> for Tree<A>
where
    A: Array<Item = U>,
{
    type Output = [U];

    fn index(&self, index: Range<usize>) -> &Self::Output {
        &self.vec[index]
    }
}

#[derive(Copy, Clone, Debug)]
pub struct MaybeIndex {
    index: isize,
}

impl From<MaybeIndex> for Option<usize> {
    fn from(index: MaybeIndex) -> Option<usize> {
        if index.index == -1 {
            None
        } else {
            Some(index.index as usize)
        }
    }
}

#[derive(Clone, Debug)]
pub struct Change {
    apon: MaybeIndex,
    variant: Variant,
}

#[derive(Clone, Debug)]
pub enum Variant {
    AddDel {
        del_apon_eq: Range<usize>,
        del_eq_amt: usize,
    },

    DelAfterAdd {
        occurrence_count: usize,
        next_occurrence: usize,
    },
}
