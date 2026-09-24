//! Argon2id over the C ABI (okay-rust, specs/polyglot-rust.md): one function,
//! plain pointers and lengths, an integer answer. No allocation crosses the
//! boundary: the caller owns every buffer, including the output.

use argon2::{Algorithm, Argon2, Params, Version};
use std::slice;

/// Derive `out_len` bytes of Argon2id (version 0x13) from `password` and `salt`.
///
/// Answers 0 when `out` holds the hash; -1 for a null pointer where bytes are
/// owed; -2 for parameters Argon2 refuses (its own bounds); -3 when hashing fails.
#[no_mangle]
pub extern "C" fn okay_argon2id(
    password: *const u8, password_len: usize,
    salt: *const u8, salt_len: usize,
    memory_kib: u32, iterations: u32, parallelism: u32,
    out: *mut u8, out_len: usize,
) -> i32 {
    if (password.is_null() && password_len > 0) || salt.is_null() || out.is_null() {
        return -1;
    }
    // SAFETY: the caller hands buffers of exactly these lengths, alive for the call
    let password = if password_len == 0 { &[][..] } else { unsafe { slice::from_raw_parts(password, password_len) } };
    let salt = unsafe { slice::from_raw_parts(salt, salt_len) };
    let out = unsafe { slice::from_raw_parts_mut(out, out_len) };
    let params = match Params::new(memory_kib, iterations, parallelism, Some(out_len)) {
        Ok(p) => p,
        Err(_) => return -2,
    };
    match Argon2::new(Algorithm::Argon2id, Version::V0x13, params).hash_password_into(password, salt, out) {
        Ok(()) => 0,
        Err(_) => -3,
    }
}

/// `n` bytes of this module's memory, for the host to fill before a call
/// (the WebAssembly road: a host cannot pass its own pointers in). 0 when
/// `n` is 0; a trap when memory cannot grow.
#[no_mangle]
pub extern "C" fn okay_alloc(n: usize) -> *mut u8 {
    let mut v = Vec::<u8>::with_capacity(n);
    let p = v.as_mut_ptr();
    std::mem::forget(v);
    p
}

/// give back what `okay_alloc(n)` answered
#[no_mangle]
pub extern "C" fn okay_free(p: *mut u8, n: usize) {
    if !p.is_null() {
        unsafe { drop(Vec::from_raw_parts(p, 0, n)) }
    }
}
