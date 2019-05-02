#![crate_type = "cdylib"]

#[macro_use]
extern crate lazy_static;

use std::mem;
use std::ptr::NonNull;
use std::panic;
use std::sync::Mutex;
use std::str;

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

objects, 001 LSB

*/

const OBJECT_MASK: usize = 0b111;
const OBJECT_TAG: usize = 0b001;

const RODATA_SIZE: usize = 4096;

lazy_static! {
    static ref RODATAS: Mutex<Vec<Box<[u8; RODATA_SIZE]>>> = {
        Mutex::new(Vec::new())
    };
}

enum Object {
    Pair(Val, Val),
    Close(usize, usize, Vec<Val>),
    Slot(Val),
    String(String),
}

#[derive(PartialEq,Clone,Copy,Debug)]
pub struct Val(usize);

fn tag<T>(b: Box<T>) -> Val {
    unsafe {
        let raw = Box::into_raw(b);
        let ptr: usize = mem::transmute(raw);
        assert!(ptr & OBJECT_MASK == 0);

        let tagged = ptr | OBJECT_TAG;
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

fn is_tagged(v: Val) -> bool {
    unsafe {
        let ptr: usize = mem::transmute(v);
        return (ptr & OBJECT_MASK) == OBJECT_TAG;
    }
}

impl Val {
    fn is_pair(&self) -> bool {
        if !is_tagged(*self) {
            return false;
        }

        let obj: &Object = untag(*self);
        match obj {
            Object::Pair(car, cdr) => true,
            _ => false,
        }
    }
    fn is_close(&self) -> bool {
        if !is_tagged(*self) {
            return false;
        }

        let obj: &Object = untag(*self);
        match obj {
            Object::Close(index, size, frees) => true,
            _ => false,
        }
    }
    fn is_slot(&self) -> bool {
        if !is_tagged(*self) {
            return false;
        }

        let obj: &Object = untag(*self);
        match obj {
            Object::Slot(val) => true,
            _ => false,
        }
    }
    fn is_string(&self) -> bool {
        if !is_tagged(*self) {
            return false;
        }

        let obj: &Object = untag(*self);
        match obj {
            Object::String(str) => true,
            _ => false,
        }
    }

    fn int(n: usize) -> Val {
        Val(n << 2)
    }

    fn unwrap<'a>(self) -> &'a mut Object {
        untag(self)
    }

    fn slot(val: Val) -> Val {
        tag(Box::new(Object::Slot(val)))
    }
    fn pair(car: Val, cdr: Val) -> Val {
        tag(Box::new(Object::Pair(car, cdr)))
    }
    fn close(index: usize, size: usize) -> Val {
        tag(Box::new(Object::Close(index, size, Vec::with_capacity(size))))
    }
    fn string_from_rodata(id: usize, offset: usize, size: usize) -> Val {
        let rodatas = RODATAS.lock().unwrap();
        assert!(id < rodatas.len());
        let rodata = &rodatas[id];

        let end = offset + size;
        let s = str::from_utf8(&rodata[offset..end]).unwrap();

        tag(Box::new(Object::String(s.to_string())))
    }
}

#[no_mangle]
pub fn alloc_rodata() -> usize {
    let mut rodatas = RODATAS.lock().unwrap();
    let rodata: [u8; RODATA_SIZE] = [0; RODATA_SIZE];
    let id = rodatas.len();
    rodatas.push(Box::new(rodata));
    id
}

#[no_mangle]
pub fn get_rodata_offset(id: usize) -> usize {
    let rodatas = RODATAS.lock().unwrap();

    assert!(id < rodatas.len());
    let rodata = &rodatas[id];
    unsafe {
        return mem::transmute(rodata.as_ptr());
    }
}

#[no_mangle]
pub fn alloc_slot(val: Val) -> Val {
    Val::slot(val)
}

#[no_mangle]
pub fn unslot(val: Val) -> Val {
    let obj = val.unwrap();
    match obj {
        Object::Slot(val) => *val,
        _ => panic!()
    }
}

#[no_mangle]
pub fn alloc_pair(car: Val, cdr: Val) -> Val {
    Val::pair(car, cdr)
}

#[no_mangle]
pub fn car(val: Val) -> Val {
    let obj = val.unwrap();
    match obj {
        Object::Pair(car, cdr) => *car,
        _ => panic!()
    }
}

#[no_mangle]
pub fn cdr(val: Val) -> Val {
    let obj = val.unwrap();
    match obj {
        Object::Pair(car, cdr) => *cdr,
        _ => panic!()
    }
}

#[no_mangle]
pub fn alloc_close(index: usize, size: usize) -> Val {
    Val::close(index, size)
}

#[no_mangle]
pub fn store_free(close_val: Val, index: usize, val: Val) -> Val {
    let obj = close_val.unwrap();
    match obj {
        Object::Close(findex, size, frees) => {
            assert!(index == frees.len());
            frees.push(val);
            close_val
        },
        _ => panic!(),
    }
}

#[no_mangle]
pub fn get_free(close_val: Val, index: usize) -> Val {
    let obj = close_val.unwrap();
    match obj {
        Object::Close(findex, size, frees) => frees[index],
        _ => panic!(),
    }
}

#[no_mangle]
pub fn get_close_func_index(close_val: Val) -> usize {
    let obj = close_val.unwrap();
    match obj {
        Object::Close(findex, size, frees) => *findex,
        _ => panic!(),
    }
}

#[no_mangle]
pub fn alloc_string(id: usize, offset: usize, size: usize) -> Val {
    Val::string_from_rodata(id, offset, size)
}

#[no_mangle]
pub fn get_string_length(val: Val) -> usize {
    let obj = val.unwrap();
    match obj {
        Object::String(string) => string.as_bytes().len(),
        _ => panic!(),
    }
}

#[no_mangle]
pub fn get_string_offset(val: Val) -> usize {
    let obj = val.unwrap();
    unsafe {
        match obj {
            Object::String(string) => mem::transmute(string.as_bytes().as_ptr()),
            _ => panic!(),
        }
    }
}

#[no_mangle]
pub fn is_slot(val: Val) -> bool {
    val.is_slot()
}

#[no_mangle]
pub fn is_close(val: Val) -> bool {
    val.is_close()
}

#[no_mangle]
pub fn is_pair(val: Val) -> bool {
    val.is_pair()
}

#[no_mangle]
pub fn is_string(val: Val) -> bool {
    val.is_string()
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

        let out = slot.unwrap();
        assert_eq!(out, val)
    }

    #[test]
    fn pairs() {
        let car = Val::int(123);
        let cdr = Val::int(456);
        let pair = Val::pair(car, cdr);

        assert!(pair.is_pair());
        
        let out = pair.unwrap();

        assert_eq!(car, out.car);
        assert_eq!(cdr, out.cdr);
    }
}