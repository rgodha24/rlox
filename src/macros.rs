#[macro_export]
macro_rules! convert {
    ($from:ident; $to:ident; $token:ident; $($variant:ident,)*; $($variable_variant:ident,)*) => {
            match $token {
                $($from::$variant => Ok($to::$variant),)*
                $($from::$variable_variant(s) => Ok($to::$variable_variant(s.clone())),)*
                _ => Err(()),
            }
        }
}
