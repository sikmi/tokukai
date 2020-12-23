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

fn mul_of_sum3(s: usize, n: usize, ns: &[usize]) -> Option<usize> {
    let s2 = s - n;
    let mut remains = ns[1..].to_vec();
    for _ in 1..ns.len() - 1 {
        if remains.len() == 0 {
            break;
        }
        let number = remains[0];
        let slice = remains[1..].to_vec();
        if let Some(r) = mul_of_sum(s2, number, &slice, None) {
            return Some(r * n);
        }
        remains.remove(0);
    }
    return None;
}

fn main() -> Result<(), Box<dyn std::error::Error>> {
    let content: String = fs::read_to_string("../yoshitsugu_hs/day1.txt")?;
    let mut numbers = content
        .split("\n")
        .filter_map(|s| usize::from_str(s).ok())
        .collect::<Vec<usize>>();
    numbers.sort();
    let mut result: Option<usize> = None;
    let mut remains = numbers[1..].to_vec();
    for _ in 1..numbers.len() - 1 {
        if remains.len() == 0 {
            break;
        }
        let number = remains[0];
        let slice = remains[1..].to_vec();
        result = mul_of_sum(2020, number, &slice, None);
        if result.is_some() {
            break;
        }
        remains.remove(0);
    }
    println!("{:?}", result);
    let mut result3: Option<usize> = None;
    let mut remains3 = numbers[1..].to_vec();
    for _ in 1..numbers.len() - 1 {
        if remains3.len() == 0 {
            break;
        }
        let number = remains3[0];
        let slice = remains3[1..].to_vec();
        result3 = mul_of_sum3(2020, number, &slice);
        if result3.is_some() {
            break;
        }
        remains3.remove(0);
    }
    println!("{:?}", result3);
    Ok(())
}
