macro_rules! ebnf_rhs {
    ([$e:ident]) => {
        $crate::ebnf_rhs!(opt $e);
    };
    (opt $($e:ident)|+) => {
        *($e ,)+
    };
    ($el:ident{$el2:ident}) => {
        panic!("rhs {}{}", stringify!($el), stringify!($el2));
    };
    ([$el:tt]) => {
        $crate::ebnf_rhs!(opt $el);
    };
}

#[macro_export]
macro_rules! ebnf {
    ($t:ident ::= $($elem:literal)|+) => {
        pub struct $t {
            pub inner: u8,
        }

        impl $t {
            pub fn parse(s: &str) -> $t {
                let d = s.parse::<u8>().unwrap();
                if ![ $($elem ,)* ].contains(d) {
                    panic!("parse");
                }
                $t { inner: d }
            }
        }
    };
    ($t:ident ::= $el:ident) => {
        pub fn panicc() {
            panic!("{} {}", stringify!($t), stringify!($el));
        }
    };
    //($t:ident ::= $el:ident{$el2:ident}) => {
    //    fn paniccc() {
    //        panic!("{} {} {}", stringify!($t), stringify!($el), stringify!($el2));
    //    }
    //};
    ($t:ident ::= [$($e:tt)|+]) => {
        fn panic2() {
            //let c = $crate::ebnf_rhs!([$e]);
            //$crate::ebnf_rhs!($ex);
        }
    };
    () => {
        pub fn main() {}
    };
}

#[cfg(test)]
mod test {
    #[test]
    pub fn check() {
        let s = "123";
        let d = s.parse::<u8>().unwrap();
        assert_eq!(d, 123);
    }
}
