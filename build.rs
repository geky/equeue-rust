use std::env;

fn main() {
    // EQUEUE_UTICK_WIDTH, default to u64
    println!("cargo:rerun-if-env-changed=EQUEUE_UTICK_WIDTH");
    let utick_width = env::var("EQUEUE_UTICK_WIDTH")
        .unwrap_or("64".to_owned());
    println!("cargo:rustc-cfg=equeue_utick_width=\"{}\"", utick_width);

    // EQUEUE_UDEPTR_WIDTH, default to u64
    println!("cargo:rerun-if-env-changed=EQUEUE_UDEPTR_WIDTH");
    let udeptr_width = env::var("EQUEUE_UDEPTR_WIDTH")
        .unwrap_or("64".to_owned());
    println!("cargo:rustc-cfg=equeue_udeptr_width=\"{}\"", udeptr_width);
}
