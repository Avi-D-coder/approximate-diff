use std::cmp::max;
use std::ops::Index;

pub trait Diffable<'l, I> {
    fn hash_diff(self, new: Self) -> Diff<'l, I>;
}

pub struct Diff<'l, I> {
    old: &'l [I],
    new: &'l [I],
}

pub enum Change<'l, I> {
    Added(&'l [I]),
    Removed(&'l [I]),
}

impl<'l, I> Diffable<'l, I> for &'l [I] where {
    fn hash_diff(self, new: Self) -> Diff<'l, I> {
        Diff { old: self, new }
    }
}

impl<'l, I> Iterator for Diff<'l, I>
where
    I: PartialEq,
{
    type Item = Change<'l, I>;
    fn next(&mut self) -> Option<Change<'l, I>> {
        let Self { old, new } = self;
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
            return None;
        }

        let diff_end = old
            .iter()
            .zip(new.iter())
            .enumerate()
            .rfind(|(_, (o, n))| o != n)
            .map_or(0, |(l, _)| l + 1);

        unimplemented!()
    }
}

#[test]
fn vec_diff() {
    let o = vec![1, 2, 3, 4, 5];
    let n = vec![1, 2, 3, 4, 5];

    o.as_slice().hash_diff(n.as_slice());
}
