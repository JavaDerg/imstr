use crate::data::Data;
use crate::string::{CharIndices, Chars, ImString};
use alloc::string::String;
use core::str::FromStr;
use nom::{
    AsBytes, Compare, CompareResult, ExtendInto, FindSubstring, Input, Needed, Offset, ParseTo,
};
#[cfg(test)]
use nom::{Err, error::ErrorKind};

/// Test that the specified function behaves the same regardless of whether the type is `&str` or
/// `ImString`.
#[cfg(test)]
macro_rules! test_equivalence {
    ($input:expr, |$name:ident: $type:path $(, $extra:path)*| $body:tt) => {{
        fn test<'a>($name: impl $type $(+ $extra)* + PartialEq<&'a str> + std::fmt::Debug) {
            $body
        }

        let input = $input;

        println!("Testing {input:?} for &str");
        test(input);

        println!("Testing {input:?} for ImString<Arc<String>>");
        test(ImString::<std::sync::Arc<String>>::from(input));

        println!("Testing {input:?} for ImString<Rc<String>>");
        test(ImString::<std::rc::Rc<String>>::from(input));

        println!("Testing {input:?} for ImString<Box<String>>");
        test(ImString::<std::boxed::Box<String>>::from(input));
    }};
}

impl<S: Data<String>> Input for ImString<S> {
    type Item = char;
    type Iter = Chars<S>;
    type IterIndices = CharIndices<S>;

    fn input_len(&self) -> usize {
        self.len()
    }

    fn take(&self, index: usize) -> Self {
        self.slice(..index)
    }

    fn take_from(&self, index: usize) -> Self {
        self.slice(index..)
    }

    fn take_split(&self, index: usize) -> (Self, Self) {
        (self.slice(index..), self.slice(..index))
    }

    fn position<P>(&self, predicate: P) -> Option<usize>
    where
        P: Fn(Self::Item) -> bool,
    {
        self.as_str().find(predicate)
    }

    fn iter_elements(&self) -> Self::Iter {
        self.chars()
    }

    fn iter_indices(&self) -> Self::IterIndices {
        self.char_indices()
    }

    fn slice_index(&self, count: usize) -> Result<usize, Needed> {
        let mut cnt = 0;
        for (index, _) in self.char_indices() {
            if cnt == count {
                return Ok(index);
            }
            cnt += 1;
        }
        if cnt == count {
            return Ok(self.len());
        }
        Err(Needed::Unknown)
    }
}

#[test]
fn test_input_take() {
    test_equivalence!("this is some string", |string: Input| {
        assert_eq!(string.take(0), "");
        assert_eq!(string.take(4), "this");
        assert_eq!(string.take(19), "this is some string");

        assert_eq!(string.take_split(0).1, "");
        assert_eq!(string.take_split(0).0, "this is some string");

        assert_eq!(string.take_split(4).1, "this");
        assert_eq!(string.take_split(4).0, " is some string");

        assert_eq!(string.take_split(7).1, "this is");
        assert_eq!(string.take_split(7).0, " some string");

        assert_eq!(string.take_split(12).1, "this is some");
        assert_eq!(string.take_split(12).0, " string");

        assert_eq!(string.take_split(19).1, "this is some string");
        assert_eq!(string.take_split(19).0, "");
    });
}

#[test]
fn test_input_length() {
    test_equivalence!("this is some string", |string: Input| {
        assert_eq!(string.input_len(), 19);
    });

    test_equivalence!("", |string: Input| {
        assert_eq!(string.input_len(), 0);
    });

    test_equivalence!("string", |string: Input| {
        assert_eq!(string.input_len(), 6);
    });
}

#[test]
fn test_input_iter() {
    test_equivalence!("", |string: Input| {
        assert!(string.iter_indices().next().is_none());
        assert!(string.iter_elements().next().is_none());
        assert_eq!(string.position(|_| true), None);
        assert_eq!(string.slice_index(0), Ok(0));
        assert_eq!(string.slice_index(1), Err(Needed::Unknown));
    });

    test_equivalence!("über", |string: Input<Item = char>| {
        let indices: Vec<_> = string.iter_indices().collect();
        assert_eq!(indices, &[(0, 'ü'), (2, 'b'), (3, 'e'), (4, 'r')]);
        let chars: Vec<_> = string.iter_elements().collect();
        assert_eq!(chars, &['ü', 'b', 'e', 'r']);

        assert_eq!(string.position(|_| true), Some(0));
        assert_eq!(string.position(|c| c == 'ü'), Some(0));
        assert_eq!(string.position(|c| c == 'b'), Some(2));
        assert_eq!(string.position(|c| c == 'e'), Some(3));
        assert_eq!(string.position(|c| c == 'r'), Some(4));

        assert_eq!(string.slice_index(0), Ok(0));
        assert_eq!(string.slice_index(1), Ok(2));
        assert_eq!(string.slice_index(2), Ok(3));
        assert_eq!(string.slice_index(3), Ok(4));
        assert_eq!(string.slice_index(4), Ok(5));
        assert_eq!(string.slice_index(5), Err(Needed::Unknown));
    });
}

#[test]
fn test_input_take_at_position() {
    test_equivalence!("", |string: Input| {
        assert_eq!(
            string.split_at_position::<_, ()>(|_| true).err().unwrap(),
            Err::Incomplete(Needed::new(1))
        );

        assert_eq!(
            string
                .split_at_position1::<_, ()>(|_| true, ErrorKind::Fail)
                .err()
                .unwrap(),
            Err::Incomplete(Needed::new(1))
        );

        let result = string
            .split_at_position_complete::<_, ()>(|_| true)
            .unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1, "");

        let result = string
            .split_at_position1_complete::<_, ()>(|_| true, ErrorKind::Fail)
            .err()
            .unwrap();
        assert_eq!(result, Err::Error(()));
    });

    test_equivalence!("some input", |string: Input<Item = char>| {
        assert_eq!(
            string
                .split_at_position::<_, ()>(|c| c == 'x')
                .err()
                .unwrap(),
            Err::Incomplete(Needed::new(1))
        );

        let result = string.split_at_position::<_, ()>(|c| c == ' ').unwrap();
        assert_eq!(result.0, " input");
        assert_eq!(result.1, "some");

        assert_eq!(
            string
                .split_at_position1::<_, ()>(|c| c == 'x', ErrorKind::Fail)
                .err()
                .unwrap(),
            Err::Incomplete(Needed::new(1))
        );
        let result = string
            .split_at_position1::<_, ()>(|c| c == ' ', ErrorKind::Fail)
            .unwrap();
        assert_eq!(result.0, " input");
        assert_eq!(result.1, "some");
        assert_eq!(
            string
                .split_at_position1::<_, ()>(|c| c == 's', ErrorKind::Fail)
                .err()
                .unwrap(),
            Err::Error(())
        );

        let result = string
            .split_at_position_complete::<_, ()>(|_| true)
            .unwrap();
        assert_eq!(result.0, "some input");
        assert_eq!(result.1, "");

        let result = string
            .split_at_position_complete::<_, ()>(|c| c == ' ')
            .unwrap();
        assert_eq!(result.0, " input");
        assert_eq!(result.1, "some");

        let result = string
            .split_at_position_complete::<_, ()>(|_| false)
            .unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1, "some input");

        let result = string
            .split_at_position1_complete::<_, ()>(|_| true, ErrorKind::Fail)
            .err()
            .unwrap();
        assert_eq!(result, Err::Error(()));

        let result = string
            .split_at_position1_complete::<_, ()>(|c| c == ' ', ErrorKind::Fail)
            .unwrap();
        assert_eq!(result.0, " input");
        assert_eq!(result.1, "some");

        let result = string
            .split_at_position1_complete::<_, ()>(|_| false, ErrorKind::Fail)
            .unwrap();
        assert_eq!(result.0, "");
        assert_eq!(result.1, "some input");
    });
}

impl<S: Data<String>> Offset for ImString<S> {
    fn offset(&self, second: &Self) -> usize {
        second.raw_offset().start - self.raw_offset().start
    }
}

#[test]
fn test_offset() {
    test_equivalence!("", |string: Offset| {
        assert_eq!(string.offset(&string), 0);
    });

    test_equivalence!("hello", |string: Offset, Input| {
        assert_eq!(string.offset(&string), 0);
        assert_eq!(string.offset(&string.take_from(1)), 1);
        assert_eq!(string.offset(&string.take_from(2)), 2);
        assert_eq!(string.offset(&string.take_from(3)), 3);
        assert_eq!(string.offset(&string.take_from(4)), 4);
        assert_eq!(string.offset(&string.take_from(5)), 5);
    });
}

impl<'a, S: Data<String>> Compare<&'a str> for ImString<S> {
    fn compare(&self, t: &'a str) -> CompareResult {
        self.as_str().compare(t)
    }

    fn compare_no_case(&self, t: &'a str) -> CompareResult {
        self.as_str().compare_no_case(t)
    }
}

#[test]
fn test_compare_str() {
    test_equivalence!("", |string: Compare<&'a str>| {
        assert_eq!(string.compare(""), CompareResult::Ok);
        assert_eq!(string.compare("err"), CompareResult::Incomplete);

        assert_eq!(string.compare_no_case(""), CompareResult::Ok);
        assert_eq!(string.compare_no_case("err"), CompareResult::Incomplete);
    });

    test_equivalence!("string", |string: Compare<&'a str>| {
        assert_eq!(string.compare("string"), CompareResult::Ok);
        assert_eq!(string.compare("str"), CompareResult::Ok);
        assert_eq!(string.compare("string0"), CompareResult::Incomplete);
        assert_eq!(string.compare("var"), CompareResult::Error);

        assert_eq!(string.compare_no_case("STRING"), CompareResult::Ok);
        assert_eq!(string.compare_no_case("STR"), CompareResult::Ok);
        assert_eq!(string.compare_no_case("STRING0"), CompareResult::Incomplete);
        assert_eq!(string.compare_no_case("VAR"), CompareResult::Error);
    });
}

impl<'a, S: Data<String>> Compare<&'a [u8]> for ImString<S> {
    fn compare(&self, t: &'a [u8]) -> CompareResult {
        self.as_bytes().compare(t)
    }

    fn compare_no_case(&self, t: &'a [u8]) -> CompareResult {
        self.as_bytes().compare_no_case(t)
    }
}

#[test]
fn test_compare_bytes() {
    test_equivalence!("", |string: Compare<&'a [u8]>| {
        assert_eq!(string.compare(&[]), CompareResult::Ok);
        assert_eq!(string.compare(&[101, 108]), CompareResult::Incomplete);

        assert_eq!(string.compare_no_case(&[]), CompareResult::Ok);
        assert_eq!(
            string.compare_no_case(&[101, 108]),
            CompareResult::Incomplete
        );
    });

    test_equivalence!("string", |string: Compare<&'a [u8]>| {
        assert_eq!(
            string.compare(&[115, 116, 114, 105, 110, 103]),
            CompareResult::Ok
        );
        assert_eq!(string.compare(&[115, 116, 114]), CompareResult::Ok);
        assert_eq!(
            string.compare(&[115, 116, 114, 105, 110, 103, 100]),
            CompareResult::Incomplete
        );
        assert_eq!(string.compare(&[116, 116, 116]), CompareResult::Error);

        assert_eq!(
            string.compare_no_case(&[83, 84, 82, 73, 78, 71]),
            CompareResult::Ok
        );
        assert_eq!(string.compare_no_case(&[83, 84, 82]), CompareResult::Ok);
        assert_eq!(
            string.compare_no_case(&[83, 84, 82, 73, 78, 71, 100]),
            CompareResult::Incomplete
        );
        assert_eq!(string.compare_no_case(&[84, 84, 84]), CompareResult::Error);
    });
}

impl<'a, S: Data<String>> FindSubstring<&'a str> for ImString<S> {
    fn find_substring(&self, sub_string: &'a str) -> core::option::Option<usize> {
        self.as_str().find_substring(sub_string)
    }
}

#[test]
fn test_find_substring_str() {
    test_equivalence!("", |string: FindSubstring<&'a str>| {
        assert_eq!(string.find_substring(""), Some(0));
        assert_eq!(string.find_substring("test"), None);
    });

    test_equivalence!(" test ", |string: FindSubstring<&'a str>| {
        assert_eq!(string.find_substring("test"), Some(1));
        assert_eq!(string.find_substring("bingo"), None);
    });
}

impl<S: Data<String>> AsBytes for ImString<S> {
    fn as_bytes(&self) -> &[u8] {
        self.as_bytes()
    }
}

#[test]
fn test_as_bytes() {
    test_equivalence!("", |string: AsBytes| {
        assert_eq!(string.as_bytes(), &[]);
    });

    test_equivalence!("hello", |string: AsBytes| {
        assert_eq!(string.as_bytes(), &[104, 101, 108, 108, 111]);
    });

    test_equivalence!("über", |string: AsBytes| {
        assert_eq!(string.as_bytes(), &[195, 188, 98, 101, 114]);
    });
}

impl<S: Data<String>, R: FromStr> ParseTo<R> for ImString<S> {
    fn parse_to(&self) -> Option<R> {
        self.parse().ok()
    }
}

#[test]
fn test_parse_to() {
    test_equivalence!("", |string: ParseTo<i64>| {
        assert_eq!(string.parse_to(), None);
    });

    test_equivalence!("14", |string: ParseTo<i64>| {
        assert_eq!(string.parse_to(), Some(14));
    });

    test_equivalence!("-9", |string: ParseTo<i64>| {
        assert_eq!(string.parse_to(), Some(-9));
    });
}

impl<S: Data<String>> ExtendInto for ImString<S> {
    type Item = char;
    type Extender = String;

    #[inline]
    fn new_builder(&self) -> String {
        String::new()
    }
    #[inline]
    fn extend_into(&self, acc: &mut String) {
        acc.push_str(self);
    }
}

#[test]
fn test_extend_into() {
    test_equivalence!("accumulated string", |string: ExtendInto<
        Extender = String,
    >| {
        let mut res = string.new_builder();
        string.extend_into(&mut res);

        assert_eq!(res, "accumulated string");
    });
}
