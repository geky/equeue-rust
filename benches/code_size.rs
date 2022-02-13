#![cfg_attr(target_os="none", no_std)]
#![cfg_attr(target_os="none", no_main)]

use equeue::Equeue;
use equeue::Clock;
use equeue::Signal;
use equeue::Sema;
use equeue::Delta;


// this macro drags functions in to avoid link-time gcing (Rust is very
// aggressive about gcing), it has some overhead, but it should be relatively
// minimal
macro_rules! dont_gc_me {
    ($f:expr) => {
        unsafe { core::ptr::read_volatile($f as *const fn()) };
    }
}

// stub to enable compilation
#[derive(Debug)]
struct StubClock();

impl Clock for StubClock {
    fn now(&self) -> u32 { 0 }
}

impl Signal for StubClock {
    fn signal(&self) { unimplemented!() }
}

impl Sema for StubClock {
    fn wait(&self) { unimplemented!() }
    fn wait_timeout(&self, _: Delta) { unimplemented!() }
}

#[cfg(target_os="none")]
#[panic_handler]
fn panic(_: &core::panic::PanicInfo) -> ! {
    loop {}
}

#[cfg(target_os="none")]
#[no_mangle]
pub fn _start() {
    main()
}

fn main() {
    // these are the functions we are measuring
    dont_gc_me! { Equeue::<StubClock>::with_config }
    dont_gc_me! { drop::<Equeue<StubClock>> }
    dont_gc_me! { Equeue::<StubClock>::delta_id::<Delta> }
    dont_gc_me! { Equeue::<StubClock>::cancel }
    dont_gc_me! { Equeue::<StubClock>::dispatch_ready }
    dont_gc_me! { Equeue::<StubClock>::dispatch_for::<Delta> }
    dont_gc_me! { Equeue::<StubClock>::dispatch }
    dont_gc_me! { Equeue::<StubClock>::break_ }
    dont_gc_me! { Equeue::<StubClock>::call::<fn()> }
    dont_gc_me! { Equeue::<StubClock>::call_in::<Delta, fn()> }
    dont_gc_me! { Equeue::<StubClock>::call_every::<Delta, fn()> }
}
