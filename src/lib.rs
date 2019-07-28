#[macro_export]
macro_rules! insights_limit {
    ( $result:expr, $lines:expr, { limit($limit:literal) } ) => {
       $result = $lines[0..$limit].to_vec();
    };
    
    ( $($e:tt)* ) => { };
}    

#[macro_export]
macro_rules! insights_order {
    ( $result:expr, $lines:expr, $w:expr, { order($arg:ident, desc) } ) => {
        let f = $w
            .iter()
            .find(|(k, _, _)| *k == stringify!($arg))
            .map(|(_, _, v)| v);

       $lines.sort_by(move |a, b| {
            match f {
                Some(f) => f(b, a),
                _ => std::cmp::Ordering::Equal,
            }
       })
    };
    
    ( $($e:tt)* ) => { };
}    


#[macro_export]
macro_rules! insights_filter {
    ( $result:expr, $line:expr, $w:expr, { $fs:tt || $($rest:tt)+ } ) => { 
        let mut a = true;
        insights_filter!(a, $line, $w, $fs);

        let mut b = true;
        insights_filter!(b, $line, $w, $($rest)*);

        $result = a || b;
    };
    // {} || {}
    ( $result:expr, $line:expr, $w:expr, $fs:tt || $($rest:tt)+ ) => {
        let mut a = true;
        insights_filter!(a, $line, $w, $fs);

        let mut b = true;
        insights_filter!(b, $line, $w, $($rest)*);

        $result = a || b;
    };

    ( $result:expr, $line:expr, $w:expr, { $fs:tt && $($rest:tt)+ } ) => { 
        let mut a = true;
        insights_filter!(a, $line, $w, $fs);

        let mut b = true;
        insights_filter!(b, $line, $w, $($rest)*);

        $result = a && b;
    };
    
    // {} && {}
    ( $result:expr, $line:expr, $w:expr, $fs:tt && $($rest:tt)+ ) => {
        let mut a = true;
        insights_filter!(a, $line, $w, $fs);

        let mut b = true;
        insights_filter!(b, $line, $w, $($rest)*);

        $result = a && b;
    };

    // { filter(a, like, c) }
    ( $result:expr, $l:expr, $w:expr, { filter($var:path, $op:expr, $val:literal) } ) => {
        let a = $w
            .iter()
            .find(|(k, _, _)| *k == stringify!($var))
            .map(|(_, v, _)| v);

        $result = {
            let v: Box<std::any::Any> = Box::new($val);

            match (stringify!($op), a) {
                ("eq", Some(n)) => n($l, "eq", v),
                ("neq", Some(n)) => n($l, "neq", v),
                ("gt", Some(n)) => n($l, "gt", v),
                ("gte", Some(n)) => n($l, "gte", v),
                ("lt", Some(n)) => n($l, "lt", v),
                ("lte", Some(n)) => n($l, "lte", v),
                _ => false
            }
        };
    };

    ( $result:expr, $l:expr, $w:expr, { limit($limit:literal) } ) => { };
    
    ( $result:expr, $l:expr, $w:expr, { order($arg:ident, $dir:ident) } ) => { };
    
    ( $result:expr, $($e:expr)* ) => { $result };
}

#[macro_export]
macro_rules! insights_var {
    ( $val:expr, [] __ROOT__ $($rest:ident)* ) => { 
        {
            $val.$($rest)*
        }
    };

    ( $val:expr, [] $head:ident $($rest:ident)* ) => { 
        {
            $val.$head.$($rest)*
        }
    };
    
    ( $val:expr, [] ) => { 
        {
        }
    };
    
    ( $val:expr, $($fs:ident)* ) => {
        {
            insights_var!($val, [] $($fs)*)
        }
    };
}

#[macro_export]
macro_rules! insights_expr {
    // id as id
    (@op $elm:ty, $result:expr, $var:ident => $val:tt, $ty:ty) => {
        let a: (&str, Box<Fn(&$elm, &str, Box<std::any::Any>) -> bool>, Box<Fn(&$elm, &$elm) -> std::cmp::Ordering>) = 
            (
                stringify!($val), 
                Box::new(|e: &$elm, op: &str, t: Box<std::any::Any>| {
                    if let Some(n) = t.downcast_ref::<$ty>() {
                        let val = e.$var;
                        return match op {
                            "eq" => val == *n,
                            "neq" => val != *n,
                            "gt" => val > *n,
                            "gte" => val >= *n,
                            "lt" => val < *n,
                            "lte" => val <= *n,
                            _ => false,
                        }
                    }
                    false
                }),
                Box::new(|s: &$elm, t: &$elm| s.$var.cmp(&t.$var))
            );

        $result.push(a);
    };

    // 0 as abc
    (@op $elm:ty, $result:expr, $var:literal => $val:tt, $ty:ty) => {
        let a: (&str, Box<Fn(&$elm, &str, Box<std::any::Any>) -> bool>, Box<Fn(&$elm, &$elm) -> std::cmp::Ordering>) = 
            (
                stringify!($val), 
                Box::new(|e: &$elm, op: &str, t: Box<std::any::Any>| {
                    if let Some(n) = t.downcast_ref::<$ty>() {
                        let val = *e.get($var).unwrap();
                        return match op {
                            "eq" => val == *n,
                            "neq" => val != *n,
                            "gt" => val > *n,
                            "gte" => val >= *n,
                            "lt" => val < *n,
                            "lte" => val <= *n,
                            _ => false,
                        }
                    }
                    false
                }),
                Box::new(|s: &$elm, t: &$elm| s.get($var).cmp(&t.get($var)))
            );
        $result.push(a);
    };

    // a => b as c
    (@op $elm:ty, $result:expr, $($var:ident)* => $val:tt, $ty:ty) => {
        let a: (&str, Box<Fn(&$elm, &str, Box<std::any::Any>) -> bool>, Box<Fn(&$elm, &$elm) -> std::cmp::Ordering>) = 
            (
                stringify!($val), 
                Box::new(|e: &$elm, op: &str, t: Box<std::any::Any>| { 
                    if let Some(n) = t.downcast_ref::<$ty>() {
                        let val = insights_var!(e, $($var)*);
                        return match op {
                            "eq" => val == *n,
                            "neq" => val != *n,
                            "gt" => val > *n,
                            "gte" => val >= *n,
                            "lt" => val < *n,
                            "lte" => val <= *n,
                            _ => false,
                        }
                    }
                    false
                }),
                Box::new(|s: &$elm, t: &$elm| insights_var!(s, $($var)*).cmp(&insights_var!(t, $($var)*)))
            );
        $result.push(a);
    };

    // nothing as + last 
    ($elm:ty, $result:expr, $stack:tt) => {
        insights_expr!(@op $elm, $result, $stack => $stack);
    };

    // nothing as + rest
    ($elm:ty, $result:expr, $stack:tt, $($rest:tt)+) => {
        insights_expr!(@op $elm, $result, $stack => $stack);
        insights_expr!($elm, $result, $($rest)*);
    };

    // a as b : u64
    ($elm:ty, $result:expr, $stack:tt as $val:ident : $ty:ty) => {
        insights_expr!(@op $elm, $result, $stack => $val, $ty);
    };
    
    // a => b as c : u64
    ($elm:ty, $result:expr, $($stack:ident)=>* as $val:ident : $ty:ty) => {
        insights_expr!(@op $elm, $result, $($stack)* => $val, $ty);
    };

    // a as b: u64, c as d : u64 ...
    ($elm:ty, $result:expr, $stack:tt as $val:ident : $ty:ty , $($rest:tt)+) => {
        insights_expr!(@op $elm, $result, $stack => $val, $ty);
        insights_expr!($elm, $result, $($rest)*);
    };
    
    // a => b as c: u64, d => e as f : u64 ...
    ($elm:ty, $result:expr, $($stack:ident)=>* as $val:ident : $ty:ty, $($rest:tt)+) => {
        insights_expr!(@op $elm, $result, $($stack)* => $val, $ty);
        insights_expr!($elm, $result, $($rest)*);
    };

    // abc
    ($elm:ty, $result:expr, [ $($stack:tt),* ] $var:tt $(rest:tt)* ) => {
        $(
            insights_expr!($elm, $result, [ $var $(,$stack)* ] $($rest)* );
        )*
    };
    
    ($elm:ty, $result:expr, [$token:expr]) => { };

    ($elm:ty, $result:expr, $($tokens:tt),*) => {
        $(
            insights_expr!($elm, $result, [] $($tokens)*)
        )*
    };
}

#[macro_export]
macro_rules! insights {
    ( $w:expr, $elm:ty, fields($($e:tt)+) => $($fs:tt)=>+ ) => {
        {
            let mut exprs = Vec::new(); 
            insights_expr!($elm, exprs, $($e)*);

            let mut changed = Vec::new();
            for a in $w.iter() {
                let mut check = true;

                $(
                    insights_filter!(check, a, exprs, $fs);
                )*

                if check {
                    changed.push(a);
                }
            };
            
            $(
                insights_order!(changed, changed, exprs, $fs);
            )*
            
            $(
                insights_limit!(changed, changed, $fs);
            )*

            changed 
        }
    };

    ( $w:expr, $($(@$e:ident)? $($e2:path)?),+ => $($fs:tt)=>+ ) => {
        {
            $(
                insights_filter!($fs);
            )*
        } 
    };
}

#[cfg(test)]
mod tests {
    #[test]
    fn it_ary() {
        let out = vec![
            vec![1u64, 2, 3],
            vec![4, 5, 6],
            vec![7, 8, 9],
        ];

        let result = insights!(out, 
            Vec<u64>,
            fields(2 as fst : u64, 1 as snd : u64)
            => { { filter(fst, eq, 3u64) } || { filter(snd, eq, 9u64) } }
            => { limit(1) }
        );

        assert_eq!(result, vec![&vec![1, 2, 3]]);
    }

    #[test]
    fn it_map() {
        use std::collections::HashMap;

        let mut a = HashMap::new();
        a.insert("a", 1);
        a.insert("b", 2);
        a.insert("c", 3);
        
        let mut b = HashMap::new();
        b.insert("a", 1);
        b.insert("b", 4);

        let out = vec![a.clone(), b.clone()];

        let result = insights!(out, 
            HashMap<&'static str, u64>,
            fields("a" as a : u64, "b" as b : u64)
            => { filter(a, eq, 1u64) }
            => { order(b, desc) }
            => { limit(1) }
        );

        assert_eq!(result, vec![&b]);
    }

    #[test]
    fn it_struct() {
        #[derive(Debug, PartialEq)]
        struct Library {
            pub id: u64,
            pub name: &'static str,
        }

        #[derive(Debug, PartialEq)]
        struct Person {
            pub id: u64,
            pub library: Library,
        }

        let out = vec![
            Person { id: 1, library: Library { id: 1, name: "a" } },
            Person { id: 2, library: Library { id: 2, name: "b" } },
            Person { id: 3, library: Library { id: 3, name: "a" } },
        ];

        let result = insights!(out, 
            Person, 
            fields(library => name as name : &str, id as id : u64)
            => { filter(name, eq, "a") }
            => { filter(id, eq, 3u64) }
        );

        assert_eq!(result, vec![ 
            &Person { id: 3u64, library: Library { id: 3, name: "a" } } 
        ]);
    }
}
