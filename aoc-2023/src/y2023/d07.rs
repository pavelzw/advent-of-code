use std::cmp::Ordering;
use std::cmp::Ordering::Equal;

#[derive(Clone, Debug, PartialOrd, PartialEq, Eq, Ord)]
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
    JackOrJoker,
    Queen,
    King,
    Ace,
}

impl Card {
    fn joker_cmp(&self, other: &Self) -> Ordering {
        match (&self, &other) {
            (Card::JackOrJoker, Card::JackOrJoker) => Equal,
            (Card::JackOrJoker, _) => Ordering::Less,
            (_, Card::JackOrJoker) => Ordering::Greater,
            _ => self.traditional_cmp(other),
        }
    }

    fn traditional_cmp(&self, other: &Self) -> Ordering {
        self.cmp(other)
    }
}

#[derive(Debug, Ord, PartialOrd, PartialEq, Eq)]
enum Type {
    HighCard,
    OnePair,
    TwoPairs,
    ThreeOfAKind,
    FullHouse,
    FourOfAKind,
    FiveOfAKind,
}

fn get_num_jokers(cards: &[Card; 5]) -> usize {
    cards.iter().filter(|c| **c == Card::JackOrJoker).count()
}

fn traditional_get_card_counts(cards: [Card; 5]) -> [usize; 5] {
    let mut counts = [0; 5];
    let mut cards = cards.clone();
    cards.sort();
    let mut last_card = &cards[0];
    let mut index = 0;
    let mut count = 1;
    for i in 1..cards.len() {
        let card = &cards[i];
        if card == last_card {
            count += 1;
        } else {
            counts[index] = count;
            count = 1;
            last_card = card;
            index += 1;
        }
    }
    counts[index] = count;
    counts.sort();
    counts.reverse();
    assert_eq!(counts.iter().sum::<usize>(), 5);
    counts
}

fn joker_get_card_counts(cards: [Card; 5]) -> [usize; 5] {
    let mut counts = [0; 5];
    let mut cards = cards.clone();
    cards.sort();
    // println!("{:?}", cards);
    let cards = cards
        .iter()
        .filter(|c| **c != Card::JackOrJoker)
        .collect::<Vec<_>>();
    if cards.is_empty() {
        return [0; 5];
    }
    let mut last_card = &cards[0];
    let mut index = 0;
    let mut count = 1;
    for i in 1..cards.len() {
        let card = &cards[i];
        if card == last_card {
            count += 1;
        } else {
            counts[index] = count;
            count = 1;
            last_card = card;
            index += 1;
        }
    }
    counts[index] = count;
    counts.sort();
    counts.reverse();
    counts
}

impl Type {
    fn traditional_match_type(&self, cards: [Card; 5]) -> bool {
        let counts = traditional_get_card_counts(cards);
        match self {
            Type::HighCard => true,
            Type::OnePair => counts[0] == 2,
            Type::TwoPairs => counts[0] == 2 && counts[1] == 2,
            Type::ThreeOfAKind => counts[0] == 3,
            Type::FullHouse => counts[0] == 3 && counts[1] == 2,
            Type::FourOfAKind => counts[0] == 4,
            Type::FiveOfAKind => counts[0] == 5,
        }
    }

    fn joker_match_type(&self, cards: [Card; 5]) -> bool {
        let num_jokers = get_num_jokers(&cards);
        let counts = joker_get_card_counts(cards);
        match self {
            Type::HighCard => true,
            Type::OnePair => counts[0] + num_jokers == 2,
            Type::TwoPairs => (0..=num_jokers)
                .map(|i| counts[0] + i == 2 && counts[1] + (num_jokers - i) == 2)
                .any(|b| b),
            Type::ThreeOfAKind => counts[0] + num_jokers == 3,
            // other cases are irrelevant because then they would automatically be a four of a kind
            Type::FullHouse => (0..=num_jokers)
                .map(|i| counts[0] + i == 3 && counts[1] + (num_jokers - i) == 2)
                .any(|b| b),
            Type::FourOfAKind => counts[0] + num_jokers == 4,
            Type::FiveOfAKind => counts[0] + num_jokers == 5,
        }
    }
}

#[derive(Clone)]
struct Hand {
    cards: [Card; 5],
    bid: usize,
}

impl Hand {
    fn traditional_get_type(&self) -> Type {
        for card_type in vec![
            Type::FiveOfAKind,
            Type::FourOfAKind,
            Type::FullHouse,
            Type::ThreeOfAKind,
            Type::TwoPairs,
            Type::OnePair,
        ] {
            if card_type.traditional_match_type(self.cards.clone()) {
                return card_type;
            }
        }
        return Type::HighCard;
    }

    fn joker_get_type(&self) -> Type {
        for card_type in vec![
            Type::FiveOfAKind,
            Type::FourOfAKind,
            Type::FullHouse,
            Type::ThreeOfAKind,
            Type::TwoPairs,
            Type::OnePair,
        ] {
            if card_type.joker_match_type(self.cards.clone()) {
                return card_type;
            }
        }
        return Type::HighCard;
    }
}

fn card_from_char(c: char) -> Card {
    match c {
        'A' => Card::Ace,
        'K' => Card::King,
        'Q' => Card::Queen,
        'J' => Card::JackOrJoker,
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

impl Hand {
    fn traditional_cmp(&self, other: &Self) -> Ordering {
        let self_type = self.traditional_get_type();
        let other_type = other.traditional_get_type();
        if self_type == other_type {
            for (self_card, other_card) in self.cards.iter().zip(other.cards.iter()) {
                if self_card != other_card {
                    let test = self_card.cmp(other_card);
                    return test;
                }
            }
            Equal
        } else {
            self_type.cmp(&other_type)
        }
    }

    fn joker_cmp(&self, other: &Self) -> Ordering {
        let self_type = self.joker_get_type();
        let other_type = other.joker_get_type();
        if self_type == other_type {
            for (self_card, other_card) in self.cards.iter().zip(other.cards.iter()) {
                if self_card != other_card {
                    let test = self_card.joker_cmp(other_card);
                    return test;
                }
            }
            Equal
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
            let cards = cards.chars().map(|c| card_from_char(c)).collect::<Vec<_>>();
            let cards = [
                cards.get(0).unwrap().clone(),
                cards.get(1).unwrap().clone(),
                cards.get(2).unwrap().clone(),
                cards.get(3).unwrap().clone(),
                cards.get(4).unwrap().clone(),
            ];
            Hand { cards, bid }
        })
        .collect::<Vec<_>>();
    hands.sort_by(|a, b| a.traditional_cmp(b));
    let solution_1 = hands
        .iter()
        .enumerate()
        .map(|(i, h)| {
            let rank = i + 1;
            h.bid * rank
        })
        .sum::<usize>();
    println!("Level 1: {}", solution_1);

    hands.sort_by(|a, b| a.joker_cmp(b));
    let solution_2 = hands
        .iter()
        .enumerate()
        .map(|(i, h)| {
            let rank = i + 1;
            h.bid * rank
        })
        .sum::<usize>();
    println!("Level 2: {}", solution_2);
}
