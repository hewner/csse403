use std::marker::PhantomData;

pub trait UrlMatcher<T> {
    fn do_match<'a>(&self, s:&'a str) -> Option<(T, &'a str)>;
}

// here's the FixedWithNum struct to get you started.  You'll have to
// build the rest yourself!
pub struct FixedWidthNum {
    width : usize
}

#[test]
fn test_fixed_width_num() {

    {
        // I put these test cases in their own scopes so I don't have
        // to keep making up new variable names

        let matcher = FixedWidthNum { width : 4 };
        let (a, b) = matcher.do_match("1234hello").unwrap();

        assert_eq!(a, 1234);
        assert_eq!(b, "hello");
    }
    {
        let matcher = FixedWidthNum { width : 3 };
        let (a, b) = matcher.do_match("1234hello56").unwrap();

        assert_eq!(a, 123);
        assert_eq!(b, "4hello56");
    }
    {
        let matcher = FixedWidthNum { width : 3 };
        let (a, b) = matcher.do_match("0014hello56").unwrap();

        assert_eq!(a, 1);
        assert_eq!(b, "4hello56");
    }

    {
        let matcher = FixedWidthNum { width : 3 };
        let (a, b) = matcher.do_match("123").unwrap();

        assert_eq!(a, 123);
        assert_eq!(b, "");
    }
    {
        let matcher = FixedWidthNum { width : 3 };
        let result = matcher.do_match("hello");

        assert_eq!(result, None);
    }

    {
        let matcher = FixedWidthNum { width : 3 };
        let result = matcher.do_match("12");

        assert_eq!(result, None);
    }

    
}
// UNCOMMENT THESE OTHER TESTS AS YOU GO!
//
// #[test]
// fn test_alpha_matcher() {
//     {
//         let matcher = AlphaMatcher { };
//         let (a, b) = matcher.do_match("hello1234").unwrap();
//         assert_eq!(a, "hello");
//         assert_eq!(b, "1234");        
//     }
// 
//     {
//         let matcher = AlphaMatcher { };
//         let (a, b) = matcher.do_match("q1234").unwrap();
//         assert_eq!(a, "q");
//         assert_eq!(b, "1234");        
//     }
// 
//     {
//         let matcher = AlphaMatcher { };
//         let (a, b) = matcher.do_match("longlonglong").unwrap();
//         assert_eq!(a, "longlonglong");
//         assert_eq!(b, "");        
//     }
//     
//     {
//         let matcher = AlphaMatcher { };
//         let result = matcher.do_match("1234");
//         assert_eq!(result, None);
//     }
// 
//     {
//         let matcher = AlphaMatcher { };
//         let result = matcher.do_match("");
//         assert_eq!(result, None);
//     }
// 
//     
// }
// 
// #[test]
// fn test_string_and_then_matcher() {
//     {
//         let matcher = StringAndThen::new("http://foo.com/".to_string() , AlphaMatcher { });
//         let (a, b) = matcher.do_match("http://foo.com/hello1234").unwrap();
//         assert_eq!(a, "hello");
//         assert_eq!(b, "1234");        
//     }
//     {
//         let matcher = StringAndThen::new("http://foo.com/".to_string() , AlphaMatcher { });
//         let result = matcher.do_match("XXXXXXXXXXXXXXhello1234");
//         assert_eq!(result, None);
//     }
// 
//     {
//         // this one fails cause the alphamatcher fails
//         let matcher = StringAndThen::new("http://foo.com/".to_string() , AlphaMatcher { });
//         let result = matcher.do_match("http://foo.com/1234");
//         assert_eq!(result, None);
//     }
// }
// 
// #[test]
// fn test_agg_matcher() {
// 
//     {
//         let matcher = FixedWidthNum { width : 4 };
//         let matcher2 = AlphaMatcher { };
//         let matcher3 = AggMatcher::new(matcher, matcher2);
//         let ((a3, a4), b3) = matcher3.do_match("1234hello5").unwrap();
//         assert_eq!(1234, a3);
//         assert_eq!("hello", a4);
//         assert_eq!("5", b3);
//     }        
// 
//     {
//         let matcher = FixedWidthNum { width : 4 };
//         let matcher2 = AlphaMatcher { };
//         let matcher3 = AggMatcher::new(matcher, matcher2);
//         assert_eq!(None, matcher3.do_match("hello"));
//     }        
// 
//     {
//         let matcher = FixedWidthNum { width : 4 };
//         let matcher2 = AlphaMatcher { };
//         let matcher3 = AggMatcher::new(matcher, matcher2);
//         assert_eq!(None, matcher3.do_match("333344444"));
//     }        
// 
//     {
//         // a bigger aggregate!
//         let matcher = StringAndThen::new(
//             "/product_id/".to_string(),
//             FixedWidthNum { width : 4 });
//         let matcher2 = StringAndThen::new (
//             "/state_code/".to_string(),
//             AlphaMatcher { }
//             );
//         let matcher3 = StringAndThen::new(
//             "http://foobar.com".to_string(),
//             AggMatcher::new(matcher, matcher2)
//         );
//         let ((a3, a4), b3) = matcher3.do_match("http://foobar.com/product_id/1234/state_code/hello").unwrap();
//         assert_eq!(1234, a3);
//         assert_eq!("hello", a4);
//         assert_eq!("", b3);
//     }        
// 
//     
// }
