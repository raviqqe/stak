//! Modules in Stak Scheme.

#![no_std]

mod guard;
#[cfg(feature = "hot-reload")]
mod hot_reload;
mod module;
mod r#static;
mod universal;

pub use guard::Guard;
#[cfg(feature = "hot-reload")]
pub use hot_reload::*;
pub use module::Module;
pub use r#static::*;
pub use universal::*;

/// Includes bytecode of a R7RS Scheme module built by the
/// [`stak_build`](https://docs.rs/stak-build) crate.
///
/// With the `hot-reload` feature, the module is reloaded whenever its bytecode
/// file changes.
///
/// See the [`stak`](https://docs.rs/stak) crate's documentation for full examples.
#[cfg(feature = "hot-reload")]
#[macro_export]
macro_rules! include_module {
    ($path:literal) => {{
        static MODULE: $crate::HotReloadModule =
            $crate::HotReloadModule::new(concat!(env!("OUT_DIR"), "/src/", $path));

        $crate::UniversalModule::HotReload(&MODULE)
    }};
}

/// Includes bytecode of a R7RS Scheme module built by the
/// [`stak_build`](https://docs.rs/stak-build) crate.
///
/// With the `hot-reload` feature, the module is reloaded whenever its bytecode
/// file changes.
///
/// See the [`stak`](https://docs.rs/stak) crate's documentation for full examples.
#[cfg(not(feature = "hot-reload"))]
#[macro_export]
macro_rules! include_module {
    ($path:literal) => {
        $crate::UniversalModule::Static($crate::StaticModule::new(include_bytes!(concat!(
            env!("OUT_DIR"),
            "/src/",
            $path
        ))))
    };
}
