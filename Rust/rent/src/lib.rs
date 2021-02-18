use std::cmp::Ordering;

#[derive(Eq)]
#[derive(PartialEq)]
#[derive(PartialOrd)]
struct Order {
    start: i64,
    end  : i64,
    value: i64,
}

impl Order {
    pub fn max() -> Order{
        Order { start: std::i64::MAX, end: 0, value: 0 }
    }
}

impl Ord for Order {
    fn cmp(&self, other: &Order) -> Ordering {
        self.start.cmp(&other.start)
    }
}

impl From<Vec<i64>> for Order{
    fn from(vec: Vec<i64>) -> Order {
        Order{start:vec[0], end:vec[1], value:vec[2]}       
    }
}

trait Normal {
    fn initialize(&mut self);
}

impl Normal for Order {
    fn initialize(&mut self) {
        self.end = self.start + self.end
    }
}

impl Normal for Vec<Order> {
    fn initialize(&mut self) {
        for order in self{ order.initialize() }
    }
}
trait Value {
    fn next_compatible(&self, i:usize) -> usize;
    fn value(&mut self) -> i64;
}

impl Value for Vec<Order> {
    fn next_compatible(&self, i:usize) -> usize {
        let mut result:usize = 0;
        let mut l = i+1;
        let mut h = self.len();
        let end = self[i].end;
        while l <= h {
            let m = l + (h - l) / 2;
            if self[m].start < end {
                l = m + 1
            } else {
                result = m;
                h = m - 1
            }
        }
        result
    }

    fn value(&mut self) -> i64 {
        for i in (0..(self.len())-1).rev() {
            let j= self.next_compatible(i);
            self[i].value = std::cmp::max(self[i+1].value, self[i].value + self[j].value)
        }
        self[0].value
    }
}

fn value(mut orders: Vec<Order>) -> i64 {
    assert!(!orders.is_empty(),"empty list of orders!");
    orders.initialize();
    orders.push(Order::max());
    orders.sort();
    orders.value()
}

use std::io;
use std::str::FromStr;

fn get_i64(l:i64) -> i64 {
    let mut s = String::new();
    io::stdin().read_line(&mut s).expect("read error");
    i64::from_str(&s.trim()).expect("not an int!")
}

fn main() {
    let mut l:i64 = 0;
    let max_cases = get_i64(l);
    l += 1;
    for c in (0..max_cases) {
        let max_orders = get_i64(l);
        l += 1;
        let mut orders:Vec<Order> = vec![];
        for _i in (0..max_orders) {
            let mut s = String::new();
            io::stdin().read_line(&mut s).expect("read error!");
            l += 1;
            let mut vec = s.split_whitespace()
                .map(|x| x.parse::<i64>().expect("parse error"))
                .collect::<Vec<i64>>();
            
            orders.push(vec.into());
            }
        println!("{}", value(orders))    
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_plan_with_order_is_worth_that_order() {
        let p = vec![Order { start: 0, end:9, value: 140 }];
        assert_eq!(140, value(p));
    }

    #[test]
    fn a_plan_with_two_orders_is_worth_the_sum_of_order_values() {
        let p = vec![Order { start: 0, end:9, value: 140 }
                    ,Order { start: 9, end:5, value: 120 }];
        assert_eq!(260, value(p));
    }
    #[test]
    fn a_plan_with_four_orders() {
        let p = vec![Order { start: 0, end:5, value: 10 }
                    ,Order { start: 3, end:7, value: 13 }
                    ,Order { start: 5, end:9, value:  7 }
                    ,Order { start: 6, end:7, value:  8 }
        ];
        assert_eq!(18, value(p));
    }
    
}
