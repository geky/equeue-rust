
use core::future::Future;
use core::fmt::Debug;

use crate::Event;
use crate::Error;
use crate::Delta;
use crate::sys::*;


//// post traits ////

/// Post trait
pub trait Post {
    fn post(&mut self);
}

impl<F: FnMut()> Post for F {
    fn post(&mut self) {
        self()
    }
}

/// Post-once trait, a special case
pub trait PostOnce {
    fn post_once(self);
}

impl<F: FnOnce()> PostOnce for F {
    fn post_once(self) {
        self()
    }
}

/// Post-static trait, a post that does not reclaim memory
pub trait PostStatic<C=SysClock>: Sized {
    fn post_static(self_: Event<'_, Self, C>);
}


//// Into/From delta traits ////

pub trait TryIntoDelta<C>: Sized {
    type Error;
    fn try_into_delta(self, clock: &C) -> Result<Delta, Self::Error>;
}

pub trait TryFromDelta<C>: Sized {
    type Error;
    fn try_from_delta(clock: &C, delta: Delta) -> Result<Self, Self::Error>;
}


//// System level traits ////

/// Some way to get the time, for some definition of time
pub trait Clock: Send + Sync + Debug {
    fn now(&self) -> utick;
}

/// Locking primitive
pub trait Lock: Send + Sync + Debug {
    type Guard;
    fn lock(&self) -> Self::Guard;
}

/// Binary semaphore, aka a waiting/signalling primitive
pub trait Sema: Send + Sync + Debug {
    fn signal(&self);
    fn wait(&self, ticks: Option<Delta>);
}

/// An asynchronous binary semaphore, for waiting asynchronously
pub trait AsyncSema: Sema + Send + Sync + Debug {
    type AsyncWait: Future<Output=()>;
    fn wait_async(&self, ticks: Option<Delta>) -> Self::AsyncWait;
}

