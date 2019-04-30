#![crate_type = "cdylib"]

use std::mem;
use std::ptr::NonNull;

/*
0 = 0
1 = 1
? = any value
X = undefined

fixnums are the only tagged values with 00 LSB
fixnum  = ??????????????????????????????00

other immediates, 1111 LSB
=================================

char immediate tag = 00             VV
char    = XXXXXXXXXXXXXXXX????????XX001111

boolean immediate tag = 01          VV
boolean = XXXXXXXXXXXXXXXXXXXXXXXX?X011111

null immediate tag = 10             VV
null    = XXXXXXXXXXXXXXXXXXXXXXXXXX101111

objects, 0??? LSB

pair object tag = 001                  VVV
pair    = ?????????????????????????????001

close object tag = 010                  VVV
close    = ????????????????????????????010

slot object tag = 011                  VVV
slot    = ????????????????????????????011

string? 100
vector? 101
record? 110
*/

const OBJECT_MASK: usize = 0b111;
const PAIR_TAG: usize = 0b01;
const CLOSE_TAG: usize = 0b10;
const SLOT_TAG: usize = 0b11;

#[derive(PartialEq,Clone,Copy,Debug)]
pub struct Val(usize);

#[derive(PartialEq,Debug)]
pub struct Pair {
    car: Val,
    cdr: Val,
}

#[derive(PartialEq,Debug)]
pub struct Close {
    index: usize,
    size: usize,
    frees: Vec<Val>,
}

fn tag<T>(b: Box<T>, tag: usize) -> Val {
    unsafe {
        let raw = Box::into_raw(b);
        let ptr: usize = mem::transmute(raw);
        assert!(ptr & OBJECT_MASK == 0);

        let tagged = ptr | tag;
        return mem::transmute(tagged);
    }
}

fn untag<'a, T>(v: Val) -> &'a mut T {
    unsafe {
        let tagged: usize = mem::transmute(v);
        let ptr = tagged & (!OBJECT_MASK);
        let b: NonNull<T> = mem::transmute(ptr);
        &mut *b.as_ptr()
    }
}

impl Val {
    fn is_pair(&self) -> bool {
        self.get_object_tag() == PAIR_TAG
    }
    fn is_close(&self) -> bool {
        self.get_object_tag() == CLOSE_TAG
    }
    fn is_slot(&self) -> bool {
        self.get_object_tag() == SLOT_TAG
    }
    fn get_object_tag(&self) -> usize {
        self.0 & OBJECT_MASK
    }

    fn int(n: usize) -> Val {
        Val(n << 2)
    }

    fn unwrap_slot(self) -> Val {
        assert!(self.is_slot());
        *untag(self)
    }
    fn slot(val: Val) -> Val {
        let b = Box::new(val);
        tag(b, SLOT_TAG)
    }

    fn unwrap_pair<'a>(self) -> &'a mut Pair {
        assert!(self.is_pair());
        untag(self)
    }
    fn pair(car: Val, cdr: Val) -> Val {
        let b = Box::new(Pair {
            car: car,
            cdr: cdr,
        });
        tag(b, PAIR_TAG)
    }

    fn unwrap_close<'a>(self) -> &'a mut Close {
        assert!(self.is_close());
        untag(self)
    }
    fn close(index: usize, size: usize) -> Val {
        let b = Box::new(Close {
            index: index,
            size: size,
            frees: Vec::with_capacity(size),
        });
        tag(b, CLOSE_TAG)
    }
}

#[no_mangle]
pub fn alloc_slot(val: Val) -> Val {
    Val::slot(val)
}

#[no_mangle]
pub fn unslot(val: Val) -> Val {
    val.unwrap_slot()
}

#[no_mangle]
pub fn alloc_pair(car: Val, cdr: Val) -> Val {
    Val::pair(car, cdr)
}

#[no_mangle]
pub fn car(val: Val) -> Val {
    val.unwrap_pair().car
}

#[no_mangle]
pub fn cdr(val: Val) -> Val {
    val.unwrap_pair().cdr
}

#[no_mangle]
pub fn alloc_close(index: usize, size: usize) -> Val {
    Val::close(index, size)
}

#[no_mangle]
pub fn store_free(close_val: Val, index: usize, val: Val) -> Val {
    let close = close_val.unwrap_close();
    assert!(index == close.frees.len());
    close.frees.push(val);
    close_val
}

#[no_mangle]
pub fn get_free(close_val: Val, index: usize) -> Val {
    let close = close_val.unwrap_close();
    close.frees[index]
}

#[no_mangle]
pub fn get_close_func_index(close_val: Val) -> usize {
    let close = close_val.unwrap_close();
    close.index
}

#[cfg(test)]
mod tests {
    use super::{ Val, tag, untag, OBJECT_MASK, SLOT_TAG, CLOSE_TAG, PAIR_TAG };
    use std::mem;

    #[test]
    fn tagging_boxes() {
        let original: usize = 0b11111111111111111111111111111000;

        let pair: usize = 0b11111111111111111111111111111001;
        let close: usize = 0b11111111111111111111111111111010;
        let slot: usize = 0b11111111111111111111111111111011;

        fn bits_to_val(n: usize) -> Val {
            unsafe {
                return mem::transmute(n);
            }
        }
        fn bits_to_box(n: usize) -> Box<Val> {
            unsafe {
                return mem::transmute(n);
            }
        }
        fn val_to_bits(n: Val) -> usize {
            unsafe {
                return mem::transmute(n);
            }
        }
        fn box_to_bits(n: Box<Val>) -> usize {
            unsafe {
                return mem::transmute(n);
            }
        }

        fn tag_bits(n: usize, tag: usize) -> usize {
            val_to_bits(tag(bits_to_box(n), tag))
        }

        fn untag_bits(n: usize) -> usize {
            box_to_bits(untag(bits_to_val(n)))
        }

        assert_eq!(tag_bits(original, SLOT_TAG), slot);
        assert_eq!(tag_bits(original, PAIR_TAG), pair);
        assert_eq!(tag_bits(original, CLOSE_TAG), close);

        assert_eq!(untag_bits(slot), original);
        assert_eq!(untag_bits(pair), original);
        assert_eq!(untag_bits(close), original);
    }

    #[test]
    fn slots() {
        let val = Val::int(1234);
        let slot = Val::slot(val);

        assert!(slot.is_slot());

        let out = slot.unwrap_slot();
        assert_eq!(out, val)
    }

    #[test]
    fn pairs() {
        let car = Val::int(123);
        let cdr = Val::int(456);
        let pair = Val::pair(car, cdr);

        assert!(pair.is_pair());
        
        let out = pair.unwrap_pair();

        assert_eq!(car, out.car);
        assert_eq!(cdr, out.cdr);
    }
}