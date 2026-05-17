use std::alloc::{Layout, alloc, dealloc};
use std::hash::{Hash, Hasher};
use std::mem;
use std::ops::{Deref, DerefMut};
use std::ptr::{NonNull, null_mut};
use std::sync::atomic::{AtomicUsize, Ordering};

pub struct RcBoxInner<T> {
    strong: AtomicUsize,
    value: T,
}

pub trait CustomAllocator {
    unsafe fn allocate(&self, layout: Layout) -> *mut u8;
    unsafe fn deallocate(&self, ptr: *mut u8, layout: Layout);
}

#[derive(Clone, Copy)]
pub struct GlobalAllocator;

impl CustomAllocator for GlobalAllocator {
    #[inline]
    unsafe fn allocate(&self, layout: Layout) -> *mut u8 {
        unsafe { alloc(layout) }
    }

    #[inline]
    unsafe fn deallocate(&self, ptr: *mut u8, layout: Layout) {
        unsafe { dealloc(ptr, layout) }
    }
}

#[derive(Clone, Copy)]
pub struct NoDealloc;

impl CustomAllocator for NoDealloc {
    #[inline]
    unsafe fn allocate(&self, _: Layout) -> *mut u8 {
        null_mut()
    }

    #[inline]
    unsafe fn deallocate(&self, _: *mut u8, _: Layout) {}
}

pub struct Rc<T, A: CustomAllocator = GlobalAllocator> {
    ptr: NonNull<RcBoxInner<T>>,
    allocator: A,
}

impl<T> Rc<T, GlobalAllocator> {
    pub fn new(value: T) -> Self {
        Self::new_in(value, GlobalAllocator)
    }
}

impl<T, A: CustomAllocator> Rc<T, A> {
    pub fn new_in(value: T, allocator: A) -> Self {
        let layout = Layout::new::<RcBoxInner<T>>();

        let ptr = unsafe {
            let mem = allocator.allocate(layout);
            if mem.is_null() {
                std::alloc::handle_alloc_error(layout);
            }

            let ptr = mem as *mut RcBoxInner<T>;

            ptr.write(RcBoxInner {
                strong: AtomicUsize::new(1),
                value,
            });

            NonNull::new_unchecked(ptr)
        };

        Self { ptr, allocator }
    }

    pub fn new_in_bump(value: T, arena: &Bump) -> Rc<T, NoDealloc> {
        let layout = Layout::new::<RcBoxInner<T>>();

        let ptr = unsafe {
            let mem = arena.alloc_layout(layout);

            let ptr = mem::transmute::<NonNull<u8>, *mut RcBoxInner<T>>(mem);

            ptr.write(RcBoxInner {
                strong: AtomicUsize::new(1),
                value,
            });

            NonNull::new_unchecked(ptr)
        };

        Rc {
            ptr,
            allocator: NoDealloc {},
        }
    }

    pub fn strong_count(&self) -> usize {
        unsafe { (*self.ptr.as_ptr()).strong.load(Ordering::SeqCst) }
    }

    pub fn addr(&self) -> usize {
        self.ptr.as_ptr() as usize
    }

    pub fn ptr_cmp(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

impl<T, A: CustomAllocator + Copy> Clone for Rc<T, A> {
    fn clone(&self) -> Self {
        unsafe {
            (*self.ptr.as_ptr()).strong.fetch_add(1, Ordering::SeqCst);
        }

        Self {
            ptr: self.ptr,
            allocator: self.allocator,
        }
    }
}

impl<T, A: CustomAllocator> Deref for Rc<T, A> {
    type Target = T;

    fn deref(&self) -> &Self::Target {
        unsafe { &(*self.ptr.as_ptr()).value }
    }
}
impl<T, A: CustomAllocator> DerefMut for Rc<T, A> {
    fn deref_mut(&mut self) -> &mut Self::Target {
        unsafe { &mut (*self.ptr.as_ptr()).value }
    }
}

impl<T, A: CustomAllocator> Drop for Rc<T, A> {
    fn drop(&mut self) {
        unsafe {
            let strong = (*self.ptr.as_ptr()).strong.fetch_sub(1, Ordering::SeqCst);

            if strong == 1 {
                std::ptr::drop_in_place(&mut (*self.ptr.as_ptr()).value);

                let layout = Layout::new::<RcBoxInner<T>>();

                self.allocator
                    .deallocate(self.ptr.as_ptr() as *mut u8, layout);
            }
        }
    }
}
impl<T: PartialEq, A: CustomAllocator> PartialEq for Rc<T, A> {
    fn eq(&self, other: &Self) -> bool {
        unsafe {
            if self.ptr_cmp(other) {
                return true;
            }
            self.ptr.as_ref().value == other.ptr.as_ref().value
        }
    }
}
impl<T: PartialEq + Eq, A: CustomAllocator> Eq for Rc<T, A> {}
impl<T: Hash, A: CustomAllocator> Hash for Rc<T, A> {
    fn hash<H: Hasher>(&self, state: &mut H) {
        unsafe { self.ptr.as_ref().value.hash(state) }
    }
}
use std::fmt;

use bumpalo::Bump;
impl<T: fmt::Debug, A: CustomAllocator> fmt::Debug for Rc<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", unsafe { &self.ptr.as_ref().value })
    }
}
