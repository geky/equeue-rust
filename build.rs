use std::env;
use std::path::PathBuf;
use std::path::Path;
use cfg_if::cfg_if;

fn main() {
    // override EQUEUE_SYS_PATH
    println!("cargo:rerun-if-env-changed=EQUEUE_SYS_PATH");
    let mut sys_path = env::var_os("EQUEUE_SYS_PATH")
        .map(PathBuf::from)
        .unwrap_or_else(|| {
            cfg_if! {
                if #[cfg(feature="loom")] {
                    PathBuf::from("sys/loom.rs")
                } else {
                    PathBuf::from("sys/std.rs")
                }
            }
        });
    // convert from crate relative
    if sys_path.is_relative() {
        sys_path = Path::new("..").join(sys_path);
    }
    println!("cargo:rustc-env=EQUEUE_SYS_PATH={}", sys_path.display());

    // override EQUEUE_UTICK_WIDTH
    println!("cargo:rerun-if-env-changed=EQUEUE_UTICK_WIDTH");
    let utick_width = env::var("EQUEUE_UTICK_WIDTH")
        .unwrap_or_else(|_| {
            cfg_if! {
                if #[cfg(feature="utick-at-least-u128")] {
                    "128".to_owned()
                } else if #[cfg(feature="utick-at-least-u64")] {
                    "64".to_owned()
                } else if #[cfg(feature="utick-at-least-u32")] {
                    "32".to_owned()
                } else if #[cfg(feature="utick-at-least-u16")] {
                    "16".to_owned()
                } else if #[cfg(feature="utick-at-least-u8")] {
                    "8".to_owned()
                } else {
                    // default to u32, this is fairly arbitrary, but
                    // it's the cheap option on 32-bit MCUs
                    "32".to_owned()
                }
            }
        });
    println!("cargo:rustc-cfg=equeue_utick_width=\"{}\"", utick_width);

    // override EQUEUE_UDEPTR_WIDTH
    println!("cargo:rerun-if-env-changed=EQUEUE_UDEPTR_WIDTH");
    let udeptr_width = env::var("EQUEUE_UDEPTR_WIDTH")
        .unwrap_or_else(|_| {
            cfg_if! {
                if #[cfg(feature="udeptr-at-least-u128")] {
                    "128".to_owned()
                } else if #[cfg(feature="udeptr-at-least-u64")] {
                    "64".to_owned()
                } else if #[cfg(feature="udeptr-at-least-u32")] {
                    // note we can't go lower than u32, we need to be able to
                    // break the udeptr type into 3 parts
                    "32".to_owned()
                } else {
                    // default to usize, udeptr and usize aren't necessarily
                    // related, but we assume 32-bit systems don't need more
                    // than 4*2^16 = 256 KiB of events
                    "native".to_owned()
                }
            }
        });
    println!("cargo:rustc-cfg=equeue_udeptr_width=\"{}\"", udeptr_width);

    // override EQUEUE_QUEUE_MODE
    println!("cargo:rerun-if-env-changed=EQUEUE_QUEUE_MODE");
    match env::var("EQUEUE_QUEUE_MODE").as_deref() {
        Ok("lockless") | Err(_) => {
            println!("cargo:rustc-cfg=equeue_queue_mode=\"lockless\"");
        }
        Ok("locking") => {
            println!("cargo:rustc-cfg=equeue_queue_mode=\"locking\"");
        }
        Ok(mode) => {
            panic!("equeue: unknown equeue_queue_mode {:?}?", mode);
        }
    }

    // override EQUEUE_ALLOC_MODE, default to same behavior as EQUEUE_QUEUE_MODE
    println!("cargo:rerun-if-env-changed=EQUEUE_ALLOC_MODE");
    match 
        env::var("EQUEUE_ALLOC_MODE")
            .or_else(|_| env::var("EQUEUE_QUEUE_MODE"))
            .as_deref()
    {
        Ok("lockless") | Err(_) => {
            println!("cargo:rustc-cfg=equeue_alloc_mode=\"lockless\"");
        }
        Ok("locking") => {
            println!("cargo:rustc-cfg=equeue_alloc_mode=\"locking\"");
        }
        Ok(mode) => {
            panic!("equeue: unknown equeue_alloc_mode {:?}?", mode);
        }
    }

    // override EQUEUE_BREAK_MODE, default to same behavior as EQUEUE_QUEUE_MODE
    println!("cargo:rerun-if-env-changed=EQUEUE_BREAK_MODE");
    match
        env::var("EQUEUE_ALLOC_MODE")
            .or_else(|_| env::var("EQUEUE_QUEUE_MODE"))
            .as_deref()
    {
        Ok("lockless") | Err(_) => {
            println!("cargo:rustc-cfg=equeue_break_mode=\"lockless\"");
        }
        Ok("locking") => {
            println!("cargo:rustc-cfg=equeue_break_mode=\"locking\"");
        }
        Ok(mode) => {
            panic!("equeue: unknown equeue_break_mode {:?}?", mode);
        }
    }
}
