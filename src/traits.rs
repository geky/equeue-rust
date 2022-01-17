
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
pub trait PostStatic: Sized {
    fn post_static(self_: Event<'_, Self>);
}



//// Into/From delta traits ////

///// A trait for converting to a delta
//pub trait TryIntoDelta {
//    fn try_into_delta(self) -> Result<itick, Error>;
//}
//
///// A trait for converting from a delta
//pub trait FromDelta {
//    fn from_delta(t: itick) -> Self;
//}


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
pub trait AsyncSema: Send + Sync + Debug {
    type AsyncWait: Future<Output=()>;
    fn wait_async(&self, ticks: Option<Delta>) -> Self::AsyncWait;
}

