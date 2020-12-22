use std::cmp::min;
use std::fs;
use std::str::FromStr;

fn mul_of_sum(s: usize, n: usize, ns: &[usize], previous: Option<usize>) -> Option<usize> {
    let pivot = (ns.len() as f32 / 2.0 as f32).ceil() as usize;
    let pivot = min(&ns.len() - 1, pivot);
    if let Some(prev) = previous {
        if prev == pivot {
            return None;
        }
    }
    match ns.get(pivot) {
        Some(n2) => {
            if *n2 + n == s {
                return Some(n * *n2);
            } else if *n2 + n > s {
                return mul_of_sum(s, n, &ns[0..pivot], Some(pivot));
            } else {
                return mul_of_sum(s, n, &ns[pivot..], Some(pivot));
            }
        }
        _ => None,
    }
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content: String = fs::read_to_string("../ynishi/day1.data")?;
    let mut numbers = content
        .split("\n")
        .filter_map(|s| usize::from_str(s).ok())
        .collect::<Vec<usize>>();
    numbers.sort();
    let mut result: Option<usize> = None;
    let mut remains = numbers[1..].to_vec();
    for index in 1..numbers.len() - 1 {
        let number = remains[index];
        let slice = remains
            .iter()
            .filter_map(|r| if *r != number { Some(*r) } else { None })
            .collect::<Vec<usize>>();
        result = mul_of_sum(2020, number, &slice, None);
        if result.is_some() {
            break;
        }
        remains.remove(0);
    }
    println!("{:?}", result);
    Ok(())
}
