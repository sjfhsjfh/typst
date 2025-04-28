use typst_macros::{cast, elem, Cast};
use typst_syntax::Spanned;

use crate::{
    diag::{error, HintedStrResult, SourceResult},
    engine::Engine,
    foundations::{
        self, dict, Content, Decimal, Dict, NativeElement, Packed, Show, Smart,
        ToDecimal, Value,
    },
    text::TextElem,
};

/// An element that displays a number in a certain style.
#[elem(Show)]
pub struct DigitsElem {
    #[required]
    #[parse(
        Decimal::construct(engine, args.expect::<Spanned<ToDecimal>>("value")?)?
    )]
    pub value: Decimal,

    #[default]
    pub strategy: RoundingStrategy,

    #[default]
    pub grouping: Smart<GroupingStrategy>,

    #[default]
    pub precision: Option<RoundingPrecision>,

    #[default]
    pub decimal_marker: Smart<Content>,

    #[default]
    pub grouping_marker: Smart<Content>,

    #[default]
    pub trailing_decimal_marker: bool,

    #[default]
    pub numerals: Smart<Numerals>,
}

impl DigitsElem {
    pub fn packed(value: impl Into<Decimal>) -> Content {
        Self::new(value.into()).pack()
    }
}

impl Show for Packed<DigitsElem> {
    fn show(
        &self,
        _engine: &mut Engine,
        styles: foundations::StyleChain,
    ) -> SourceResult<Content> {
        let mut realized = Content::empty();
        let span = self.span();
        let rounding_st: rust_decimal::RoundingStrategy = self.strategy(styles).into();
        let grouping_st = self
            .grouping(styles)
            .custom()
            .unwrap_or(GroupingStrategy { grouping: 3 });
        let pos = self.precision(styles);
        let rounded = if let Some(precision) = pos {
            match precision {
                RoundingPrecision::DecimalPoint(dp) => {
                    self.value.round_dp_with_strategy(dp, rounding_st)
                }
                RoundingPrecision::SignificantFigures(sf) => self
                    .value
                    .round_sf_with_strategy(sf, rounding_st)
                    .ok_or(vec![error!(span, "Decimal rounding sf out of range")])?,
            }
        } else {
            self.value
        };
        let _lang = TextElem::lang_in(styles);
        let _region = TextElem::region_in(styles);
        // TODO: Locale
        let component_itrtr = DigitsIterator::new(grouping_st, rounded);
        // TODO: Locale
        let decimal_marker = self
            .decimal_marker(styles)
            .custom()
            .unwrap_or(TextElem::new(".".into()).pack());
        // TODO: Locale
        let grouping_marker = self
            .grouping_marker(styles)
            .custom()
            .unwrap_or(TextElem::new(" ".into()).pack());
        let trailing_dm = self.trailing_decimal_marker(styles);
        // TODO: Locale
        let numerals = self.numerals(styles).custom().unwrap_or(Numerals::Western);

        let mut counting = false;
        let mut counter = 0u32;
        for (dead, component) in component_itrtr {
            if !counting {
                match pos {
                    Some(RoundingPrecision::DecimalPoint(_)) => {
                        if matches!(component, DigitsComponent::DecimalPoint) {
                            counting = true;
                        }
                    }
                    Some(RoundingPrecision::SignificantFigures(_)) => {
                        if matches!(component, DigitsComponent::Digit(digit) if digit != 0)
                        {
                            counting = true;
                        }
                    }
                    _ => {}
                }
            } else if matches!(component, DigitsComponent::Digit(_)) {
                counter += 1;
            }
            if !(trailing_dm && matches!(component, DigitsComponent::DecimalPoint))
                && match pos {
                    None => dead,
                    Some(RoundingPrecision::DecimalPoint(dp)) => {
                        counting && counter >= dp
                    }
                    Some(RoundingPrecision::SignificantFigures(sf)) => {
                        dead && counting && counter >= sf
                    }
                }
            {
                break;
            }
            match component {
                DigitsComponent::Digit(digit) => {
                    realized +=
                        TextElem::new(numerals.digit_to_char(digit).into()).pack();
                }
                DigitsComponent::DecimalPoint => {
                    realized += decimal_marker.clone();
                }
                DigitsComponent::GroupingSeparator => {
                    realized += grouping_marker.clone();
                }
            }
        }
        Ok(realized.spanned(span))
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum RoundingPrecision {
    DecimalPoint(u32),
    SignificantFigures(u32),
}

impl RoundingPrecision {
    pub fn parse_dict(dict: &mut Dict) -> HintedStrResult<Self> {
        match (dict.contains("decimal-point"), dict.contains("significant-figures")) {
            (true, false) => {
                Ok(RoundingPrecision::DecimalPoint(dict.take("decimal-point")?.cast()?))
            }
            (false, true) => Ok(RoundingPrecision::SignificantFigures(
                dict.take("significant-figures")?.cast()?,
            )),
            _ => Err(
                "Rounding position must be either decimal-point or significant-figures"
                    .into(),
            ),
        }
    }
}

cast! {
    RoundingPrecision,
    self => Value::Dict(self.into()),
    mut dict: Dict => {
        RoundingPrecision::parse_dict(&mut dict)?
    }
}

impl From<RoundingPrecision> for Dict {
    fn from(value: RoundingPrecision) -> Self {
        match value {
            RoundingPrecision::DecimalPoint(pos) => dict! { "decimal-point" => pos },
            RoundingPrecision::SignificantFigures(pos) => {
                dict! { "significant-figures" => pos }
            }
        }
    }
}

/// Rounding rules for formatting numbers.
///
/// See [rust_decimal::RoundingStrategy]
#[derive(Default, Debug, Clone, Copy, Hash, PartialEq, Eq, Cast)]
pub enum RoundingStrategy {
    #[default]
    MidpointNearestEven,
    MidpointAwayFromZero,
    MidpointTowardZero,
    ToZero,
    AwayFromZero,
    ToNegativeInfinity,
    ToPositiveInfinity,
}

impl From<RoundingStrategy> for rust_decimal::RoundingStrategy {
    fn from(value: RoundingStrategy) -> Self {
        match value {
            RoundingStrategy::MidpointNearestEven => Self::MidpointNearestEven,
            RoundingStrategy::MidpointAwayFromZero => Self::MidpointAwayFromZero,
            RoundingStrategy::MidpointTowardZero => Self::MidpointTowardZero,
            RoundingStrategy::ToZero => Self::ToZero,
            RoundingStrategy::AwayFromZero => Self::AwayFromZero,
            RoundingStrategy::ToNegativeInfinity => Self::ToNegativeInfinity,
            RoundingStrategy::ToPositiveInfinity => Self::ToPositiveInfinity,
        }
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub struct GroupingStrategy {
    pub grouping: usize,
}

cast! {
    GroupingStrategy,
    self => Value::Dict(self.into()),
    mut dict: Dict => {
        let grouping = dict.take("grouping")?.cast()?;
        if grouping == 0 {
            return Err("Grouping must be greater than 0".into());
        }
        GroupingStrategy { grouping }
    }
}

impl From<GroupingStrategy> for Dict {
    fn from(value: GroupingStrategy) -> Self {
        dict! { "grouping" => value.grouping }
    }
}

pub struct DigitsIterator {
    /// The decimal point flag
    fract: bool,

    grouping: GroupingStrategy,

    pub current: Decimal,

    /// Reversed, "1,000" => [0, 0, 0, Sep, 1]
    integer_part: Vec<DigitsComponent>,

    group_counter: usize,
}

impl Iterator for DigitsIterator {
    type Item = (bool, DigitsComponent);

    fn next(&mut self) -> Option<Self::Item> {
        let dead = self.is_dead();
        if !self.fract {
            if let Some(item) = self.integer_part.pop() {
                return Some((dead, item));
            } else {
                self.fract = true;
                self.group_counter = 0;
                return Some((dead, DigitsComponent::DecimalPoint));
            }
        }
        if self.group_counter == self.grouping.grouping {
            self.group_counter = 0;
            return Some((dead, DigitsComponent::GroupingSeparator));
        }
        self.group_counter += 1;
        self.current = self.current.checked_mul(Decimal::TEN)?;
        let digit = self.current.trunc().to_string().parse().ok()?;
        self.current = self.current.fract();
        Some((dead, DigitsComponent::Digit(digit)))
    }
}

impl DigitsIterator {
    pub fn new(grouping: GroupingStrategy, digits: Decimal) -> Self {
        let mut group_counter = 0;
        let integer_part = digits
            .trunc()
            .abs()
            .to_string()
            .chars()
            .rev()
            .flat_map(|c| {
                group_counter = if group_counter == 0 {
                    grouping.grouping - 1
                } else {
                    group_counter - 1
                };
                let digit = c.to_digit(10).unwrap() as u8;
                let mut res = vec![DigitsComponent::Digit(digit)];
                if group_counter == 0 {
                    res.push(DigitsComponent::GroupingSeparator);
                }
                res
            })
            .collect::<Vec<_>>();
        let current = digits.fract();
        Self {
            current,
            fract: false,
            grouping,
            group_counter,
            integer_part,
        }
    }

    pub fn is_dead(&self) -> bool {
        self.integer_part.is_empty() && self.current.is_zero()
    }
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq)]
pub enum DigitsComponent {
    /// A digit (0-9)
    Digit(u8),

    /// A decimal point ('.' in 3.14)
    DecimalPoint,

    /// A grouping separator (',' in 1,000)
    GroupingSeparator,
}

#[derive(Debug, Clone, Copy, Hash, PartialEq, Eq, Cast)]
pub enum Numerals {
    Eastern,
    Western,
}

impl Numerals {
    const EASTERN: [char; 10] = [
        '\u{0660}', '\u{0661}', '\u{0662}', '\u{0663}', '\u{0664}', '\u{0665}',
        '\u{0666}', '\u{0667}', '\u{0668}', '\u{0669}',
    ];
    const WESTERN: [char; 10] = ['0', '1', '2', '3', '4', '5', '6', '7', '8', '9'];
    pub fn digit_to_char(&self, digit: u8) -> char {
        match self {
            Numerals::Eastern => Self::EASTERN[digit as usize],
            Numerals::Western => Self::WESTERN[digit as usize],
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_digits() {
        let digits = Decimal::from(-1000i64);
        let grouping = GroupingStrategy { grouping: 3 };
        let mut iter = DigitsIterator::new(grouping, digits);
        assert_eq!(iter.next(), Some((false, DigitsComponent::Digit(1))));
        assert_eq!(iter.next(), Some((false, DigitsComponent::GroupingSeparator)));
        assert_eq!(iter.next(), Some((false, DigitsComponent::Digit(0))));
        assert_eq!(iter.next(), Some((false, DigitsComponent::Digit(0))));
        assert_eq!(iter.next(), Some((false, DigitsComponent::Digit(0))));
        assert_eq!(iter.next(), Some((true, DigitsComponent::DecimalPoint)));
        assert_eq!(iter.next(), Some((true, DigitsComponent::Digit(0))));
        assert_eq!(iter.next(), Some((true, DigitsComponent::Digit(0))));
        assert_eq!(iter.next(), Some((true, DigitsComponent::Digit(0))));
        assert_eq!(iter.next(), Some((true, DigitsComponent::GroupingSeparator)));
    }
}
