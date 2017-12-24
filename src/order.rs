
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct OrderId(pub u8);

pub mod id {
    use super::OrderId;
    pub const DIE: OrderId = OrderId(0);
}
