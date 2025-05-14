const SIGN_MASK: u32 = 0b10000000000000000000000000000000;

pub fn parse_int(string: String, radix: u64) -> Option<i32> {
    let mut negative = false;
    let mut number = if let Some(stripped) = string.strip_prefix("-") {
        negative = true;
        stripped.to_string()
    } else {
        string
    };
    number = if let Some(stripped) = number.strip_prefix("0x") {
        stripped.to_string()
    } else {
        number
    };
    let mut bytes = u32::from_str_radix(&number, radix as u32).ok()?;
    let value = if ((bytes & SIGN_MASK) >> 31) == 1 {
        negative = !negative;
        bytes = (bytes << 1) >> 1;
        (i32::MAX - bytes as i32) + 1
    } else {
        bytes as i32
    };
    if negative {
        Some(-value)
    } else {
        Some(value)
    }
}
