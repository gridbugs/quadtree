#![allow(dead_code)]
extern crate rand;
use rand::Rng;
use std::num::NonZeroUsize;

type Number = i64;
type Index = NonZeroUsize;

fn half(n: Number) -> Number {
    n / 2
}

fn two_power(n: u32) -> Number {
    2_i64.pow(n)
}

fn usize_to_index(n: usize) -> Index {
    NonZeroUsize::new(n).unwrap()
}

fn index_to_usize(n: Index) -> usize {
    n.get()
}

fn max_number(a: Number, b: Number) -> Number {
    Number::max(a, b)
}

fn rand_number<R: Rng>(rng: &mut R) -> Number {
    rng.gen()
}

fn number_zero() -> Number {
    0
}

fn number_max() -> Number {
    ::std::i64::MAX / 2
}

#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash)]
struct BoundingRect {
    centre_x: Number,
    centre_y: Number,
    width: Number,
    height: Number,
}

impl BoundingRect {
    fn new_with_nw_and_size(x: Number, y: Number, width: Number, height: Number) -> Self {
        assert!(width > number_zero());
        assert!(height > number_zero());
        Self {
            centre_x: x + half(width),
            centre_y: y + half(height),
            width,
            height,
        }
    }
    fn pretty(&self) -> String {
        const DIV: Number = 10000000000000000;
        let x = self.centre_x - self.width / 2;
        let y = self.centre_y - self.height / 2;
        format!(
            "[ ({}, {}) ; {} x {}]",
            x / DIV,
            y / DIV,
            self.width / DIV,
            self.height / DIV
        )
    }
    fn max_dimension(&self) -> Number {
        Number::max(self.width, self.height)
    }
    fn max_centre(&self) -> Number {
        Number::max(self.centre_x, self.centre_y)
    }
    fn is_intersecting(&self, other: &Self) -> bool {
        let dx = (self.centre_x - other.centre_x).abs();
        let dy = (self.centre_y - other.centre_y).abs();
        dx + dx < self.width + other.width && dy + dy < self.height + other.height
    }
    fn intersects_node_square(
        &self,
        centre_x: Number,
        centre_y: Number,
        size: Number,
    ) -> bool {
        let dx = (centre_x - self.centre_x).abs();
        let dy = (centre_y - self.centre_y).abs();
        dx + dx < size + self.width && dy + dy < size + self.height
    }
}

struct NodeSquare {
    centre_x: Number,
    centre_y: Number,
    size: Number,
}

impl NodeSquare {
    fn new_nw_at_origin(size: Number) -> Self {
        let half_size = half(size);
        Self {
            centre_x: half_size,
            centre_y: half_size,
            size,
        }
    }
}

#[derive(Debug)]
struct Node<T> {
    nw: Option<Index>,
    ne: Option<Index>,
    sw: Option<Index>,
    se: Option<Index>,
    data: Vec<(BoundingRect, T)>,
}

impl<T> Node<T> {
    fn empty() -> Self {
        Self {
            nw: None,
            ne: None,
            sw: None,
            se: None,
            data: Vec::new(),
        }
    }
    fn empty_with_nw(nw: Index) -> Self {
        Self {
            nw: Some(nw),
            ne: None,
            sw: None,
            se: None,
            data: Vec::new(),
        }
    }
}

#[derive(Debug)]
struct Quadtree<T> {
    nodes: Vec<Node<T>>,
    size: Number,
}

impl<T> Quadtree<T> {
    fn new(size_exp: u32) -> Self {
        Self {
            nodes: vec![Node::empty()],
            size: two_power(size_exp),
        }
    }

    fn double_size(&mut self) {
        self.size = self.size + self.size;
        let end_position = self.nodes.len();
        let new_root = Node::empty_with_nw(usize_to_index(end_position));
        self.nodes.push(new_root);
        let old_root = self.nodes.swap_remove(0);
        self.nodes.push(old_root);
    }

    fn grow_to_size(&mut self, target_size: Number) {
        while self.size < target_size {
            self.double_size();
        }
    }

    fn insert(&mut self, bounding_rect: BoundingRect, value: T) {
        let max_dimension = bounding_rect.max_dimension();
        self.grow_to_size(max_number(max_dimension, bounding_rect.max_centre()));
        let mut node_square = NodeSquare::new_nw_at_origin(self.size);
        let mut index = 0_usize;
        let mut num_nodes = self.nodes.len();
        let node = loop {
            let (next_node_square, next_index, should_push) = {
                let node = &mut self.nodes[index];
                let next_node_square_size = half(node_square.size);
                if next_node_square_size < max_dimension {
                    break node;
                }
                let offset = half(next_node_square_size);
                let (next_node_square_centre_x, next_node_square_centre_y, next_child) =
                    if bounding_rect.centre_x < node_square.centre_x {
                        if bounding_rect.centre_y < node_square.centre_y {
                            (
                                node_square.centre_x - offset,
                                node_square.centre_y - offset,
                                &mut node.nw,
                            )
                        } else {
                            (
                                node_square.centre_x - offset,
                                node_square.centre_y + offset,
                                &mut node.sw,
                            )
                        }
                    } else {
                        if bounding_rect.centre_y < node_square.centre_y {
                            (
                                node_square.centre_x + offset,
                                node_square.centre_y - offset,
                                &mut node.ne,
                            )
                        } else {
                            (
                                node_square.centre_x + offset,
                                node_square.centre_y + offset,
                                &mut node.se,
                            )
                        }
                    };
                let node_square = NodeSquare {
                    centre_x: next_node_square_centre_x,
                    centre_y: next_node_square_centre_y,
                    size: next_node_square_size,
                };
                let (index, should_push) = match *next_child {
                    Some(index) => (index, false),
                    None => {
                        let next_index = usize_to_index(num_nodes);
                        *next_child = Some(next_index);
                        (next_index, true)
                    }
                };
                (node_square, index, should_push)
            };
            if should_push {
                self.nodes.push(Node::empty());
                num_nodes += 1;
            }
            node_square = next_node_square;
            index = index_to_usize(next_index);
        };
        node.data.push((bounding_rect, value));
    }

    fn for_each_intersecting_rec<F>(
        nodes: &Vec<Node<T>>,
        index: usize,
        node_square: NodeSquare,
        bounding_rect: &BoundingRect,
        f: &mut F,
    ) where
        F: FnMut(&BoundingRect, &T),
    {
        let node = &nodes[index];
        for &(ref stored_bounding_box, ref value) in node.data.iter() {
            if bounding_rect.is_intersecting(stored_bounding_box) {
                f(stored_bounding_box, value);
            }
        }
        let size = half(node_square.size);
        let offset = half(size);
        let north_y = node_square.centre_y - offset;
        let south_y = node_square.centre_y + offset;
        let west_x = node_square.centre_x - offset;
        let east_x = node_square.centre_x + offset;
        if let Some(index) = node.nw {
            if bounding_rect.intersects_node_square(west_x, north_y, size) {
                Self::for_each_intersecting_rec(
                    nodes,
                    index_to_usize(index),
                    NodeSquare {
                        centre_x: west_x,
                        centre_y: north_y,
                        size,
                    },
                    bounding_rect,
                    f,
                );
            }
        }
        if let Some(index) = node.ne {
            if bounding_rect.intersects_node_square(east_x, north_y, size) {
                Self::for_each_intersecting_rec(
                    nodes,
                    index_to_usize(index),
                    NodeSquare {
                        centre_x: east_x,
                        centre_y: north_y,
                        size,
                    },
                    bounding_rect,
                    f,
                );
            }
        }
        if let Some(index) = node.sw {
            if bounding_rect.intersects_node_square(west_x, south_y, size) {
                Self::for_each_intersecting_rec(
                    nodes,
                    index_to_usize(index),
                    NodeSquare {
                        centre_x: west_x,
                        centre_y: south_y,
                        size,
                    },
                    bounding_rect,
                    f,
                );
            }
        }
        if let Some(index) = node.se {
            if bounding_rect.intersects_node_square(east_x, south_y, size) {
                Self::for_each_intersecting_rec(
                    nodes,
                    index_to_usize(index),
                    NodeSquare {
                        centre_x: east_x,
                        centre_y: south_y,
                        size,
                    },
                    bounding_rect,
                    f,
                );
            }
        }
    }

    fn for_each_intersecting<F>(&self, bounding_rect: &BoundingRect, mut f: F)
    where
        F: FnMut(&BoundingRect, &T),
    {
        Self::for_each_intersecting_rec(
            &self.nodes,
            0,
            NodeSquare::new_nw_at_origin(self.size),
            bounding_rect,
            &mut f,
        );
    }

    fn naive_for_each_intersecting<F>(&self, bounding_rect: &BoundingRect, mut f: F)
    where
        F: FnMut(&BoundingRect, &T),
    {
        for node in self.nodes.iter() {
            for &(ref stored_bounding_box, ref value) in node.data.iter() {
                if bounding_rect.is_intersecting(stored_bounding_box) {
                    f(stored_bounding_box, value);
                }
            }
        }
    }
}

struct Naive<T> {
    data: Vec<(BoundingRect, T)>,
}

impl<T> Naive<T> {
    fn new() -> Self {
        Self {
            data: Default::default(),
        }
    }
    fn insert(&mut self, bounding_rect: BoundingRect, value: T) {
        self.data.push((bounding_rect, value));
    }
    fn for_each_intersecting<F>(&self, bounding_rect: &BoundingRect, mut f: F)
    where
        F: FnMut(&BoundingRect, &T),
    {
        for &(ref stored_bounding_box, ref value) in self.data.iter() {
            if bounding_rect.is_intersecting(stored_bounding_box) {
                f(stored_bounding_box, value);
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use rand::{Rng, SeedableRng, StdRng};
    use std::collections::HashSet;

    #[test]
    fn resizing() {
        let mut qt = Quadtree::new(4);
        qt.insert(BoundingRect::new_with_nw_and_size(200, 4, 1, 2), ());
        assert_eq!(qt.size, 256);
    }

    fn make_rng() -> impl Rng {
        StdRng::from_seed([0; 32])
    }

    fn rand_bounding_box<R: Rng>(rng: &mut R) -> BoundingRect {
        let width = rand_number(rng).abs() % number_max();
        let height = rand_number(rng).abs() % number_max();
        let x = rand_number(rng).abs() % (number_max() - width);
        let y = rand_number(rng).abs() % (number_max() - height);
        BoundingRect::new_with_nw_and_size(x, y, width, height)
    }

    #[test]
    fn example() {
        let mut loose_quadtree = Quadtree::new(1);
        loose_quadtree.insert(BoundingRect::new_with_nw_and_size(157, 100, 216, 192), ());
        let mut vec = Vec::new();
        let query = BoundingRect::new_with_nw_and_size(33, 270, 408, 37);
        loose_quadtree.for_each_intersecting(&query, |_, _| vec.push(()));
        panic!("{:#?}", loose_quadtree);
    }

    fn compare_to_naive() {
        const NUM_VALUES: usize = 35;
        const NUM_QUERIES: usize = 2;
        let mut rng = make_rng();
        let mut naive = Naive::new();
        let mut loose_quadtree = Quadtree::new(1);
        for i in 0..NUM_VALUES {
            let bounding_box = rand_bounding_box(&mut rng);
            naive.insert(bounding_box.clone(), i);
            loose_quadtree.insert(bounding_box, i);
        }
        let mut naive_values = HashSet::new();
        let mut loose_quadtree_values = HashSet::new();
        for i in 0..NUM_QUERIES {
            naive_values.clear();
            loose_quadtree_values.clear();
            let bounding_box = rand_bounding_box(&mut rng);
            naive.for_each_intersecting(&bounding_box, |bounding_rect, &value| {
                naive_values.insert((bounding_rect.clone(), value));
            });
            loose_quadtree.for_each_intersecting(
                &bounding_box,
                |bounding_rect, &value| {
                    loose_quadtree_values.insert((bounding_rect.clone(), value));
                },
            );
            let diff = naive_values
                .difference(&loose_quadtree_values)
                .collect::<Vec<_>>();
            if !diff.is_empty() {
                let pretty_diff =
                    diff.iter().map(|(b, _)| b.pretty()).collect::<Vec<_>>();
                panic!("{}\n{:#?}\n{:#?}", i, bounding_box.pretty(), pretty_diff);
            }
        }
    }
}
