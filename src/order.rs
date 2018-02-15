
#[derive(Copy, Clone, Eq, PartialEq)]
pub struct OrderId(pub u8);

pub mod id {
    #![allow(dead_code)]
    use super::OrderId;
    pub const DIE: OrderId = OrderId(0);
    pub const TRAIN: OrderId = OrderId(0x26);
    pub const HARVEST_GAS: OrderId = OrderId(0x53);
    pub const RETURN_GAS: OrderId = OrderId(0x54);
    pub const RETURN_MINERALS: OrderId = OrderId(0x5a);
    pub const RESET_COLLISION_HARVESTER: OrderId = OrderId(0x97);
}
