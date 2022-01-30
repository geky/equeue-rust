
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
pub trait PostStatic<E>: Sized {
    fn post_static(self_: E);
}


//// Into/From delta traits ////

pub trait TryIntoDelta: Sized {
    type Error;
    fn try_into_delta(self, frequency: utick) -> Result<Delta, Self::Error>;
}

pub trait TryFromDelta: Sized {
    type Error;
    fn try_from_delta(delta: Delta, frequency: utick) -> Result<Self, Self::Error>;
}


//// System level traits ////

/// Some way to get the time, for some definition of time
pub trait Clock: Send + Sync + Debug {
    fn now(&self) -> utick;
    fn frequency(&self) -> utick;
}

/// Locking primitive
pub trait Lock: Send + Sync + Debug {
    type Guard;
    fn lock(&self) -> Self::Guard;
}

/// Common signal trait for semaphores, this is separate since we
/// need a single signal function for both Sema and AsyncSema
pub trait Signal: Send + Sync + Debug {
    fn signal(&self);
}

/// Binary semaphore, aka a waiting/signalling primitive
pub trait Sema: Signal + Send + Sync + Debug {
    fn wait(&self);
    fn wait_timeout(&self, ticks: Delta);
}

/// An asynchronous binary semaphore, for waiting asynchronously
pub trait AsyncSema: Signal + Send + Sync + Debug {
    type AsyncWait: Future<Output=()>;
    type AsyncWaitTimeout: Future<Output=()>;

    fn wait_async(&self) -> Self::AsyncWait;
    fn wait_timeout_async(&self, ticks: Delta) -> Self::AsyncWaitTimeout;
}

