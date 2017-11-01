struct Order {
    start: i64,
    end  : i64,
    value: i64,
}

fn next_compatible(i:usize, orders:&Vec<Order>) -> usize {
    let mut result:usize = 0;
    let mut l = i+1;
    let mut h = orders.len();
    let end = orders[i].end;
    while l <= h {
        let m = l + (h - l) / 2;
        if orders[m].start < end {
            l = m + 1
        } else {
            result = m;
            h = m - 1
        }
    }
    result
}

fn value(mut orders: Vec<Order>) -> i64 {
    if orders.len() > 0 {
        const ORDER_MAX:Order = Order { start: std::i64::MAX, end: 0, value:0 };

        for i in (0..orders.len()) {
            orders[i].end = orders[i].start + orders[i].end
        };
        orders.push(ORDER_MAX);
        orders.sort_by(|a,b| a.start.cmp(&b.start));

        for i in (0..(orders.len())-1).rev() {
            let j= next_compatible(i, &orders);
            orders[i].value = std::cmp::max(orders[i+1].value, orders[i].value + orders[j].value)
        };
        orders[0].value
    }
    else {
        0 
    }
}

use std::io;
use std::str::FromStr;

fn get_i64(l:i64) -> i64 {
    let mut s = String::new();
    io::stdin().read_line(&mut s).expect("read error");
    let p = i64::from_str(&s.trim());
    match p {
        Ok(n) => n,
        Err(m) => { println!("not an int! at line{}:{}",l,s); 0 }
    }
}

fn main() {
    let mut l:i64 = 0;
    let max_cases = get_i64(l);
    l += 1;
    for c in (0..max_cases) {
        let max_orders = get_i64(l);
        l += 1;
        let mut orders:Vec<Order> = vec![];
        for i in (0..max_orders) {
            let mut s = String::new();
            io::stdin().read_line(&mut s).expect("read error!");
            l += 1;
            let mut vec = s.split_whitespace()
                .map(|x| x.parse::<i64>().expect("parse error"))
                .collect::<Vec<i64>>();
            
            orders.push(Order{start:vec[0], end:vec[1], value:vec[2]});
            }
        println!("{}", value(orders))    
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn a_plan_with_zero_orders_is_worth_zero() {
        let mut p = vec![];
        assert_eq!(0, value(p));
    }

    #[test]
    fn a_plan_with_order_is_worth_that_order() {
        let mut p = vec![Order { start: 0, end:9, value: 140 }];
        assert_eq!(140, value(p));
    }

    #[test]
    fn a_plan_with_two_orders_is_worth_the_sum_of_order_values() {
        let mut p = vec![Order { start: 0, end:9, value: 140 }
                    ,Order { start: 9, end:5, value: 120 }];
        assert_eq!(260, value(p));
    }
    #[test]
    fn a_plan_with_four_orders() {
        let mut p = vec![Order { start: 0, end:5, value: 10 }
                    ,Order { start: 3, end:7, value: 13 }
                    ,Order { start: 5, end:9, value:  7 }
                    ,Order { start: 6, end:7, value:  8 }
        ];
        assert_eq!(18, value(p));
    }
    
}
