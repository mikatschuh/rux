use std::alloc::{alloc, dealloc, Layout};
use std::hash::{Hash, Hasher};
use std::marker::PhantomData;
use std::mem::{self, MaybeUninit};
use std::ops::{Deref, DerefMut, Index, IndexMut};
use std::ptr::{null_mut, NonNull};
use std::sync::atomic::{AtomicUsize, Ordering};

// Die innere Struktur, die gezählt wird
pub struct RcBoxInner<T> {
    // Anzahl der starken Referenzen
    strong: AtomicUsize,
    // Der eigentliche Wert
    value: T,
}

// Trait für eigene Allocator-Implementierungen
pub trait CustomAllocator {
    unsafe fn allocate(&self, layout: Layout) -> *mut u8;
    unsafe fn deallocate(&self, ptr: *mut u8, layout: Layout);
}

// Implementierung des Standard-Allocators
#[derive(Clone, Copy)]
pub struct GlobalAllocator;

impl CustomAllocator for GlobalAllocator {
    #[inline]
    unsafe fn allocate(&self, layout: Layout) -> *mut u8 {
        alloc(layout)
    }

    #[inline]
    unsafe fn deallocate(&self, ptr: *mut u8, layout: Layout) {
        dealloc(ptr, layout)
    }
}

// Implementierung des Standard-Allocators
#[derive(Clone, Copy)]
pub struct MocAllocator;

impl CustomAllocator for MocAllocator {
    #[inline]
    unsafe fn allocate(&self, _: Layout) -> *mut u8 {
        null_mut()
    }

    #[inline]
    unsafe fn deallocate(&self, _: *mut u8, _: Layout) {}
}

// Unser öffentlicher Rc-Typ
pub struct Rc<T, A: CustomAllocator = GlobalAllocator> {
    // Pointer zur inneren Struktur
    ptr: NonNull<RcBoxInner<T>>,
    // Speichere den Allocator direkt
    allocator: A,
}
// Implementierung mit GlobalAllocator als Standard
impl<T> Rc<T, GlobalAllocator> {
    // Standardkonstruktor
    pub fn new(value: T) -> Self {
        Self::new_in(value, GlobalAllocator)
    }
}

// Implementierung für alle Allocator-Typen
impl<T, A: CustomAllocator> Rc<T, A> {
    // Konstruktor mit spezifischem Allocator
    pub fn new_in(value: T, allocator: A) -> Self {
        // Layout für den inneren Typ berechnen
        let layout = Layout::new::<RcBoxInner<T>>();

        // Speicher allozieren
        let ptr = unsafe {
            let mem = allocator.allocate(layout);
            if mem.is_null() {
                std::alloc::handle_alloc_error(layout);
            }

            let ptr = mem as *mut RcBoxInner<T>;

            // Innere Struktur im allozierten Speicher initialisieren
            ptr.write(RcBoxInner {
                strong: AtomicUsize::new(1),
                value,
            });

            NonNull::new_unchecked(ptr)
        };

        Self { ptr, allocator }
    }

    pub fn new_in_bump(value: T, arena: &Bump) -> Rc<T, MocAllocator> {
        let layout = Layout::new::<RcBoxInner<T>>();

        let ptr = unsafe {
            let mem = arena.alloc_layout(layout);

            let ptr = mem::transmute::<_, *mut RcBoxInner<T>>(mem);

            ptr.write(RcBoxInner {
                strong: AtomicUsize::new(1),
                value,
            });

            NonNull::new_unchecked(ptr)
        };

        Rc {
            ptr,
            allocator: MocAllocator {},
        }
    }
    // Aktuelle Anzahl der starken Referenzen
    #[allow(dead_code)]
    pub fn strong_count(&self) -> usize {
        unsafe { (*self.ptr.as_ptr()).strong.load(Ordering::SeqCst) }
    }

    #[allow(dead_code)]
    pub fn ptr_cmp(&self, other: &Self) -> bool {
        self.ptr == other.ptr
    }
}

// Eine einzige Clone-Implementierung für MyRc mit Copy-Trait-Bound
impl<T, A: CustomAllocator + Copy> Clone for Rc<T, A> {
    fn clone(&self) -> Self {
        unsafe {
            // Erhöhe den Referenzzähler
            (*self.ptr.as_ptr()).strong.fetch_add(1, Ordering::SeqCst);
        }

        Self {
            ptr: self.ptr,
            allocator: self.allocator,
        }
    }
}

// Implementierung von Deref für einfachen Zugriff auf den Wert
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

// Implementierung von Drop für die Speicherfreigabe
impl<T, A: CustomAllocator> Drop for Rc<T, A> {
    fn drop(&mut self) {
        unsafe {
            // Referenzzähler verringern
            let strong = (*self.ptr.as_ptr()).strong.fetch_sub(1, Ordering::SeqCst);

            // Wenn dies die letzte Referenz war, gebe den Speicher frei
            if strong == 1 {
                // Manuell den Destruktor für den inneren Wert aufrufen
                std::ptr::drop_in_place(&mut (*self.ptr.as_ptr()).value);

                // Layout berechnen (muss dem Allokationslayout entsprechen)
                let layout = Layout::new::<RcBoxInner<T>>();

                // Speicher freigeben mit dem gespeicherten Allocator
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
use std::fmt::{self, Debug};

use bumpalo::Bump;
impl<T: fmt::Debug, A: CustomAllocator> fmt::Debug for Rc<T, A> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{:?}", unsafe { &self.ptr.as_ref().value })
    }
}

#[inline]
fn bitwise_mod(val: usize, mod_log_2: usize) -> usize {
    val & !(!0 << mod_log_2)
}

#[derive(Debug)]
pub struct ArrayQueue<T, const CAP: usize, const CAP_LOG_2: usize> {
    buffer: [MaybeUninit<T>; CAP],
    start: usize,
    len: usize,
}
impl<T: Clone, const CAP: usize, const CAP_LOG_2: usize> Clone for ArrayQueue<T, CAP, CAP_LOG_2> {
    fn clone(&self) -> Self {
        let mut new_buffer: [MaybeUninit<T>; CAP] = unsafe {
            // Unsicher initialisiert, aber wir garantieren gleich, dass wir's korrekt setzen
            MaybeUninit::uninit().assume_init()
        };

        for i in 0..self.len {
            let idx = bitwise_mod(self.start + i, CAP_LOG_2);
            // SAFETY: `idx` ist innerhalb der gültigen initialisierten Daten
            new_buffer[idx] =
                MaybeUninit::new(unsafe { self.buffer[idx].assume_init_ref().clone() });
        }

        Self {
            buffer: new_buffer,
            start: self.start,
            len: self.len,
        }
    }
}
impl<T: PartialEq + Eq, const CAP: usize, const CAP_LOG_2: usize> PartialEq
    for ArrayQueue<T, CAP, CAP_LOG_2>
{
    fn eq(&self, other: &Self) -> bool {
        if self.len != other.len {
            return false;
        }
        for idx in 0..self.len {
            if self[idx] != other[idx] {
                return false;
            }
        }
        true
    }
}
impl<T: PartialEq + Eq, const CAP: usize, const CAP_LOG_2: usize, const N: usize> PartialEq<[T; N]>
    for ArrayQueue<T, CAP, CAP_LOG_2>
{
    fn eq(&self, other: &[T; N]) -> bool {
        if self.len != other.len() {
            return false;
        }
        for idx in 0..self.len {
            if self[idx] != other[idx] {
                return false;
            }
        }
        true
    }
}
impl<T, const CAP: usize, const CAP_LOG_2: usize> ArrayQueue<T, CAP, CAP_LOG_2> {
    pub fn new() -> Self {
        Self {
            buffer: core::array::from_fn(|_| MaybeUninit::<T>::uninit()),
            start: 0,
            len: 0,
        }
    }

    pub fn push_front(&mut self, val: T) {
        debug_assert!(self.len < CAP);
        self.start = bitwise_mod(self.start.overflowing_sub(1).0, CAP_LOG_2);
        self.len += 1;
        unsafe {
            *self.buffer[self.start].as_mut_ptr() = val;
        }
    }

    pub fn push_back(&mut self, val: T) {
        debug_assert!(self.len < CAP);
        let initial_len = self.len;
        self.len += 1;

        self[initial_len] = val;
    }

    pub fn pop_front(&mut self) -> Option<T> {
        if self.len > 0 {
            let mut last = MaybeUninit::uninit();
            std::mem::swap(&mut last, &mut self.buffer[self.start]); // extract the first element

            self.start = bitwise_mod(self.start.overflowing_add(1).0, CAP_LOG_2);
            self.len -= 1;

            Some(unsafe { last.assume_init() })
        } else {
            None
        }
    }

    pub fn pop_back(&mut self) -> Option<T> {
        if self.len > 0 {
            let mut last = MaybeUninit::uninit();
            std::mem::swap(&mut last, &mut self.buffer[self.start]); // extract the first element

            self.len -= 1;

            Some(unsafe { last.assume_init() })
        } else {
            None
        }
    }

    pub fn first(&self) -> Option<&T> {
        if self.len > 0 {
            Some(unsafe { self.buffer[self.start].assume_init_ref() })
        } else {
            None
        }
    }

    pub fn last(&self) -> Option<&T> {
        if self.len > 0 {
            Some(&self[self.start + self.len - 1])
        } else {
            None
        }
    }

    pub fn is_empty(&self) -> bool {
        self.len == 0
    }

    pub fn len(&self) -> usize {
        self.len
    }
}
impl<T, const CAP: usize, const CAP_LOG_2: usize> Index<usize> for ArrayQueue<T, CAP, CAP_LOG_2> {
    type Output = T;
    #[inline]
    fn index(&self, index: usize) -> &Self::Output {
        debug_assert!(index < self.len);
        unsafe { self.buffer[bitwise_mod(self.start + index, CAP_LOG_2)].assume_init_ref() }
        // The bitwise is a shorthand for: x % CAP
    }
}
impl<T, const CAP: usize, const CAP_LOG_2: usize> IndexMut<usize>
    for ArrayQueue<T, CAP, CAP_LOG_2>
{
    #[inline]
    fn index_mut(&mut self, index: usize) -> &mut Self::Output {
        debug_assert!(index < self.len);
        unsafe { self.buffer[bitwise_mod(self.start + index, CAP_LOG_2)].assume_init_mut() }
        // The bitwise is a shorthand for: x % CAP
    }
}

#[derive(PartialEq, Eq)]
pub struct Ref<'recv, T> {
    _marker: PhantomData<&'recv ()>,
    ptr: NonNull<T>,
}

impl<'recv, T> Clone for Ref<'recv, T> {
    fn clone(&self) -> Self {
        Self {
            _marker: PhantomData::default(),
            ptr: self.ptr,
        }
    }
}

impl<'recv, T> Copy for Ref<'recv, T> {}

impl<'recv, T: Debug> Debug for Ref<'recv, T> {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        unsafe { write!(f, "{:?}", *self.ptr.as_ptr()) }
    }
}

impl<'recv, T> Ref<'recv, T> {
    #[inline]
    pub fn new(val: &'recv mut T) -> Self {
        Self {
            _marker: PhantomData,
            ptr: unsafe { NonNull::new_unchecked(val as *mut T) },
        }
    }

    pub fn write(self, val: T) {
        unsafe {
            *self.ptr.as_ptr() = val;
        }
    }
}

#[test]
fn test() {
    let mut queue: ArrayQueue<i32, 16, 4> = ArrayQueue::new();
    assert_eq!(queue, []);
    assert_eq!(queue.first(), None);

    queue.push_back(1);
    assert_eq!(queue, [1]);

    queue.push_back(2);
    queue.push_back(3);
    assert_eq!(queue, [1, 2, 3]);
    assert_eq!(queue.first(), Some(&1));

    assert_eq!(queue.pop_front(), Some(1));
    assert_eq!(queue.pop_front(), Some(2));
    assert_eq!(queue.pop_front(), Some(3));

    // wrapping test:

    let mut queue: ArrayQueue<i32, 2, 1> = ArrayQueue::new();
    assert_eq!(queue, []);
    assert_eq!(queue.first(), None);

    queue.push_back(1);
    assert_eq!(queue, [1]);

    queue.push_back(2);
    assert_eq!(queue, [1, 2]);
    assert_eq!(queue.first(), Some(&1));

    assert_eq!(queue.pop_front(), Some(1));

    queue.push_back(3);
    assert_eq!(queue, [2, 3]);

    assert_eq!(queue.pop_front(), Some(2));
}
