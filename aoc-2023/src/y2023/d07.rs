use std::cmp::Ordering;
use std::cmp::Ordering::Equal;

#[derive(Clone)]
#[derive(Debug)]
#[derive(PartialOrd)]
#[derive(PartialEq)]
#[derive(Eq)]
#[derive(Ord)]
enum Card {
    Two,
    Three,
    Four,
    Five,
    Six,
    Seven,
    Eight,
    Nine,
    Ten,
    Jack,
    Queen,
    King,
    Ace,
}

impl Card {
    fn traditional_order(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }

    fn joker_order(&self, other: &Self) -> Ordering {
        match (self, other) {
            (Card::Jack, Card::Jack) => Equal,
            (Card::Jack, _) => Ordering::Less,
            (_, Card::Jack) => Ordering::Greater,
            _ => self.traditional_order(other),
        }
    }
}

#[derive(Debug)]
#[derive(Ord)]
#[derive(PartialOrd)]
#[derive(PartialEq)]
#[derive(Eq)]
enum Type {
    HighCard,
    OnePair,
    TwoPairs,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

impl Type {
    fn match_type(&self, cards: [Card; 5]) -> bool {
        match self {
            Type::HighCard => true,
            Type::OnePair => {
                let mut cards = cards.clone();
                cards.sort();
                let mut pairs = 0;
                for i in 0..cards.len() - 1 {
                    if cards[i] == cards[i + 1] {
                        pairs += 1;
                    }
                }
                pairs == 1
            },
            Type::TwoPairs => {
                let mut cards = cards.clone();
                cards.sort();
                let mut pairs = 0;
                for i in 0..cards.len() - 1 {
                    if cards[i] == cards[i + 1] {
                        pairs += 1;
                    }
                }
                pairs == 2
            },
            Type::ThreeOfAKind => {
                let mut cards = cards.clone();
                cards.sort();
                let mut three_of_a_kind = false;
                for i in 0..cards.len() - 2 {
                    if cards[i] == cards[i + 1] && cards[i + 1] == cards[i + 2] {
                        three_of_a_kind = true;
                    }
                }
                three_of_a_kind
            },
            Type::FullHouse => {
                let mut cards = cards.clone();
                cards.sort();
                (cards[0] == cards[1] && cards[1] == cards[2] && cards[3] == cards[4]) ||
                    (cards[0] == cards[1] && cards[2] == cards[3] && cards[3] == cards[4])
            },
            Type::FourOfAKind => {
                let mut cards = cards.clone();
                cards.sort();
                (cards[0] == cards[1] && cards[1] == cards[2] && cards[2] == cards[3]) ||
                    (cards[1] == cards[2] && cards[2] == cards[3] && cards[3] == cards[4])
            },
            Type::FiveOfAKind => {
                let cards = cards;
                cards[0] == cards[1] && cards[1] == cards[2] && cards[2] == cards[3] && cards[3] == cards[4]
            },
        }
    }

    fn match_type_2(&self, cards: [Card; 5]) -> bool {
        match self {
            Type::HighCard => true,
            Type::OnePair => true,
            Type::TwoPairs => true,
            Type::ThreeOfAKind => true,
            Type::FullHouse => true,
            Type::FourOfAKind => true,
            Type::FiveOfAKind => true,
        }
    }
}

struct Hand {
    cards: [Card; 5],
    bid: usize,
}

impl Hand {
    fn get_type(&self) -> Type {
        for card_type in vec![
            Type::FiveOfAKind,
            Type::FourOfAKind,
            Type::FullHouse,
            Type::ThreeOfAKind,
            Type::TwoPairs,
            Type::OnePair,
        ] {
            if card_type.match_type(self.cards.clone()) {
                return card_type;
            }
        }
        return Type::HighCard;
    }
}

fn Card_from_char(c: char) -> Card {
    match c {
        'A' => Card::Ace,
        'K' => Card::King,
        'Q' => Card::Queen,
        'J' => Card::Jack,
        'T' => Card::Ten,
        '9' => Card::Nine,
        '8' => Card::Eight,
        '7' => Card::Seven,
        '6' => Card::Six,
        '5' => Card::Five,
        '4' => Card::Four,
        '3' => Card::Three,
        '2' => Card::Two,
        _ => panic!("Invalid card: {}", c),
    }
}

impl Eq for Hand {}

impl PartialEq for Hand {
    fn eq(&self, other: &Self) -> bool {
        self.cards == other.cards
    }
}

impl PartialOrd for Hand {
    fn partial_cmp(&self, other: &Self) -> Option<Ordering> {
        Some(self.cmp(other))
    }
}

impl Ord for Hand {
    fn cmp(&self, other: &Self) -> Ordering {
        let self_type = self.get_type();
        let other_type = other.get_type();
        if self_type == other_type {
            for (self_card, other_card) in self.cards.iter().zip(other.cards.iter()) {
                if self_card != other_card {
                    let test = self_card.cmp(other_card);
                    return test;
                }
            }
            Ordering::Equal
        } else {
            self_type.cmp(&other_type)
        }
    }
}

pub fn solve(input: String) {
    let mut hands = input
        .split('\n')
        .filter(|l| !l.is_empty())
        .map(|l| {
            let cards = &l[0..5];
            let bid = l[6..].parse::<usize>().unwrap();
            let cards = cards
                .chars()
                .map(|c| Card_from_char(c))
                .collect::<Vec<_>>();
            let cards = [
                cards.get(0).unwrap().clone(),
                cards.get(1).unwrap().clone(),
                cards.get(2).unwrap().clone(),
                cards.get(3).unwrap().clone(),
                cards.get(4).unwrap().clone(),
            ];
            Hand {
                cards,
                bid,
            }
        }).collect::<Vec<_>>();
    hands.sort();
    let solution_1 = hands.iter().enumerate().map(|(i, h)| {
        let rank = i + 1;
        h.bid * rank
    }).sum::<usize>();
    println!("Level 1: {}", solution_1);
}