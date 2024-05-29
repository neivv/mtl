use libc::c_void;

use crate::bw;
use crate::game;

/// Call when dialog is RaceSelection
pub unsafe extern fn run_dialog_hook(
    raw: *mut c_void,
    unk: usize,
    event_handler: *mut c_void,
    orig: unsafe extern fn(*mut c_void, usize, *mut c_void) -> u32,
) -> u32 {
    let result = orig(raw, unk, event_handler);
    // Return value determines briefing room
    // 6 == protoss, 7 == terran, 8 == zerg,
    // same on vanilla and bw, even if it seems like bw campaign order
    if result == 6 || result == 7 || result == 8 {
        let game = game::get();
        let map = (**game).campaign_mission;
        if let Some(mission) = campaign_mission_from_map_id(map) {
            return match (*mission).race {
                0 => 8,
                1 => 7,
                2 | _ => 6,
            };
        } else {
            // Possibly cinematic?
            warn!("Couldn't find campaign for mission {:x}", map);
        }
    }
    result
}

fn campaign_mission_from_map_id(map: u16) -> Option<*mut bw::CampaignMission> {
    let campaign = crate::config::campaign()?;
    for &list in campaign.campaigns.iter() {
        unsafe {
            let mut mission = list;
            while (*mission).name_index != 0 {
                if map == (*mission).campaign_mission && (*mission).cinematic == 0 {
                    return Some(mission);
                }
                mission = mission.add(1);
            }
        }
    }
    None
}
