extern crate lazy_static;

use lazy_static::lazy_static;
use rand::seq::SliceRandom;
use rand::thread_rng;
use std::collections::HashMap;

const RIGHT: u16 = 8;
const BOTTOM: u16 = 4;
const LEFT: u16 = 2;
const TOP: u16 = 1;
const END: u16 = 136;
const STRAIGHT: u16 = 170;
const BEND: u16 = 204;
//const W: usize = 6;
//const H: usize = 5;
const BACKGROUNDS: [(Colour, u32); 2] = [(Blue, 12), (Yellow, 18)];
lazy_static! {
    //TODO use hashmap of <Colour, u32> to look up max for each colour
    //static ref LAST: usize = W * H - 1;
    static ref LAST: usize = BACKGROUNDS.iter().map(|v| v.1 as usize).sum::<usize>() - 1;
    //static ref HALF: u32 = ((W as f32 * H as f32) / BACKGROUNDS.len() as f32).ceil() as u32;
}


///////////////////////////////////// enums
#[derive(Debug, PartialEq, Clone, Copy)]
enum Colour {
    Blue, //NB these first six have to be in the same order as the faces are defined in the blocks array at the start of main()
    Green,
    Yellow,
    Black,
    Purple,
    Red,
    White,
    Pink,
    Lime,
    DarkPink,
    Orange
}

#[derive(PartialEq, Clone, Copy, Debug)]
enum Status {
    Edge,
    Empty,
    Occupied
}

///////////////////////////////////// for brevity in clode
use Colour::*;
use Status::*;

///////////////////////////////////// structs
#[derive(Clone, Copy, Debug)]
struct GridPosition {
    status: Status,
    block_index: usize,
    block_orientation: usize,
    block_rotation: usize,
    connectors: u16
}

impl GridPosition {
    fn new() -> GridPosition {
        GridPosition {
            status: Status::Edge,
            block_index: 0,
            block_orientation: 0,
            block_rotation: 0,
            connectors: 0
        }
    }
}

#[derive(Debug)]
struct Block {
    name: &'static str,
    faces: [Face; 6],
    location: Option<(usize, usize)> // this should correspond with block_index in GridPosition
}

impl Block {
    fn new(name: &'static str, faces: [Face; 6]) -> Block {
        Block {
            name,
            faces,
            location: None
        }
    }
}

#[derive(Debug)]
struct Face {
    background: Colour,
    foreground: Colour,
    tp: u16
}

impl Face {
    fn new(background: Colour, foreground: Colour, tp: u16) -> Face {
        Face {
            background,
            foreground,
            tp
        }
    }
}

fn as_colour(v: usize) -> &'static str {
    match v {
        0 => "Blue",
        1 => "Green",
        2 => "Yellow",
        3 => "Black",
        4 => "Purple",
        _ => "Red",
    }
}

/////////////////////////////////////
struct Puzzle {
    blocks: [Block; 30],
    chains: Vec<Vec<GridPosition>>,
    bkg_count: Vec<Vec<u32>>, //for each colour (max_num, actual_num)
}

impl Puzzle {
    fn new() -> Puzzle {
        let blocks = [
            Block::new("A", [Face::new(Blue, Yellow, END),       Face::new(Green, Yellow, END),      Face::new(Yellow, White, END),         Face::new(Black, Lime, END),        Face::new(Purple, Pink, END),          Face::new(Red, DarkPink, END)]),
            Block::new("B", [Face::new(Blue, Yellow, END),       Face::new(Green, Blue, STRAIGHT),   Face::new(Yellow, Purple, END),        Face::new(Black, DarkPink, STRAIGHT), Face::new(Purple, Black, END),         Face::new(Red, Purple, BEND)]),
            Block::new("C", [Face::new(Blue, Purple, STRAIGHT),  Face::new(Green, Black, END),       Face::new(Yellow, DarkPink, END),      Face::new(Black, Orange, STRAIGHT),   Face::new(Purple, Yellow, END),        Face::new(Red, Blue, END)]),
            Block::new("D", [Face::new(Blue, Orange, STRAIGHT),  Face::new(Green, Black, END),       Face::new(Yellow, DarkPink, STRAIGHT), Face::new(Black, White, END),         Face::new(Purple, Blue, END),          Face::new(Red, Blue, END)]),
            Block::new("E", [Face::new(Blue, Yellow, END),       Face::new(Green, White, END),       Face::new(Yellow, Green, END),         Face::new(Black, DarkPink, END),      Face::new(Purple, Green, STRAIGHT),    Face::new(Red, DarkPink, STRAIGHT)]),
            Block::new("F", [Face::new(Blue, Lime, END),         Face::new(Green, White, END),       Face::new(Yellow, Lime, END),          Face::new(Black, Green, END),         Face::new(Purple, White, END),         Face::new(Red, Green, END)]),
    
            Block::new("G", [Face::new(Blue, Pink, END),         Face::new(Green, Green, STRAIGHT),  Face::new(Yellow, White, STRAIGHT),    Face::new(Black, Blue, BEND),         Face::new(Purple, Green, END),         Face::new(Red, Yellow, STRAIGHT)]),
            Block::new("H", [Face::new(Blue, Yellow, END),       Face::new(Green, Lime, STRAIGHT), Face::new(Yellow, Lime, END),          Face::new(Black, Blue, END),          Face::new(Purple, White, END),         Face::new(Red, White, END)]),
            Block::new("I", [Face::new(Blue, Orange, STRAIGHT),  Face::new(Green, Pink, END),        Face::new(Yellow, Purple, END),        Face::new(Black, Blue, END),          Face::new(Purple, White, STRAIGHT),    Face::new(Red, DarkPink, END)]),
            Block::new("J", [Face::new(Blue, Blue, END),         Face::new(Green, Lime, END),        Face::new(Yellow, Black, STRAIGHT),    Face::new(Black, Green, END),         Face::new(Purple, Yellow, STRAIGHT),   Face::new(Red, Green, END)]),
            Block::new("K", [Face::new(Blue, Purple, END),       Face::new(Green, White, STRAIGHT),  Face::new(Yellow, Lime, STRAIGHT),     Face::new(Black, White, STRAIGHT),    Face::new(Purple, Black, END),         Face::new(Red, Purple, END)]),
            Block::new("L", [Face::new(Blue, Lime, END),         Face::new(Green, Lime, END),        Face::new(Yellow, DarkPink, END),      Face::new(Black, Blue, BEND),         Face::new(Purple, White, END),         Face::new(Red, White, BEND)]),
    
            Block::new("M", [Face::new(Blue, Lime, BEND),        Face::new(Green, Black, BEND),      Face::new(Yellow, DarkPink, END),      Face::new(Black, Pink, END),          Face::new(Purple, Yellow, END),        Face::new(Red, Blue, STRAIGHT)]),
            Block::new("N", [Face::new(Blue, Yellow, STRAIGHT),  Face::new(Green, Black, BEND),      Face::new(Yellow, Black, END),         Face::new(Black, Lime, END),        Face::new(Purple, White, END),         Face::new(Red, Yellow, END)]),
            Block::new("O", [Face::new(Blue, Orange, END),       Face::new(Green, Purple, END),      Face::new(Yellow, Orange, STRAIGHT),   Face::new(Black, Yellow, STRAIGHT),   Face::new(Purple, DarkPink, STRAIGHT), Face::new(Red, Yellow, END)]),
            Block::new("P", [Face::new(Blue, Yellow, STRAIGHT),  Face::new(Green, Green, END),       Face::new(Yellow, Lime, STRAIGHT),     Face::new(Black, Blue, END),          Face::new(Purple, Lime, END),          Face::new(Red, Yellow, END)]),
            Block::new("Q", [Face::new(Blue, Pink, END),         Face::new(Green, Yellow, END),      Face::new(Yellow, Blue, END),          Face::new(Black, Pink, END),          Face::new(Purple, Pink, STRAIGHT),     Face::new(Red, White, END)]),
            Block::new("R", [Face::new(Blue, Blue, END),         Face::new(Green, Black, END),       Face::new(Yellow, Green, STRAIGHT),    Face::new(Black, Orange, END),        Face::new(Purple, DarkPink, END),      Face::new(Red, White, END)]),
    
            Block::new("S", [Face::new(Blue, Purple, END),       Face::new(Green, Blue, END),        Face::new(Yellow, Green, END),         Face::new(Black, Orange, END),        Face::new(Purple, Orange, END),        Face::new(Red, Pink, END)]),
            Block::new("T", [Face::new(Blue, Orange, END),       Face::new(Green, Pink, STRAIGHT),   Face::new(Yellow, DarkPink, END),      Face::new(Black, Yellow, END),        Face::new(Purple, Orange, END),        Face::new(Red, Pink, END)]),
            Block::new("U", [Face::new(Blue, Green, END),        Face::new(Green, Lime, STRAIGHT),   Face::new(Yellow, Green, END),         Face::new(Black, Purple, END),        Face::new(Purple, DarkPink, STRAIGHT), Face::new(Red, White, STRAIGHT)]),
            Block::new("V", [Face::new(Blue, Pink, STRAIGHT),    Face::new(Green, Pink, END),        Face::new(Yellow, Green, STRAIGHT),    Face::new(Black, Yellow, END),        Face::new(Purple, Blue, END),          Face::new(Red, Yellow, STRAIGHT)]),
            Block::new("W", [Face::new(Blue, Orange, END),       Face::new(Green, Green, END),       Face::new(Yellow, Orange, END),        Face::new(Black, White, END),         Face::new(Purple, DarkPink, END),      Face::new(Red, Yellow, BEND)]),
            Block::new("X", [Face::new(Blue, Green, END),        Face::new(Green, Blue, END),        Face::new(Yellow, Orange, END),        Face::new(Black, Yellow, STRAIGHT),   Face::new(Purple, Black, BEND),        Face::new(Red, Purple, END)]),
    
            Block::new("Y", [Face::new(Blue, Blue, END),         Face::new(Green, Purple, END),      Face::new(Yellow, Blue, BEND),         Face::new(Black, Purple, END),        Face::new(Purple, White, STRAIGHT),    Face::new(Red, White, STRAIGHT)]),
            Block::new("Z", [Face::new(Blue, Yellow, BEND),      Face::new(Green, Purple, STRAIGHT), Face::new(Yellow, White, END),         Face::new(Black, Blue, END),          Face::new(Purple, Green, STRAIGHT),    Face::new(Red, Purple, END)]),
            Block::new("AA", [Face::new(Blue, Blue, STRAIGHT),   Face::new(Green, Yellow, BEND),     Face::new(Yellow, Green, END),         Face::new(Black, Green, STRAIGHT),    Face::new(Purple, Lime, END),          Face::new(Red, Purple, END)]),
            Block::new("BB", [Face::new(Blue, Blue, END),        Face::new(Green, Black, END),       Face::new(Yellow, Blue, END),          Face::new(Black, Lime, STRAIGHT),     Face::new(Purple, Pink, END),          Face::new(Red, White, END)]),
            Block::new("CC", [Face::new(Blue, Orange, STRAIGHT), Face::new(Green, DarkPink, END),    Face::new(Yellow, Black, END),         Face::new(Black, Purple, BEND),       Face::new(Purple, Green, END),         Face::new(Red, Yellow, END)]),
            Block::new("DD", [Face::new(Blue, Orange, END),      Face::new(Green, DarkPink, END),    Face::new(Yellow, DarkPink, STRAIGHT), Face::new(Black, DarkPink, END),      Face::new(Purple, Orange, STRAIGHT),   Face::new(Red, Pink, STRAIGHT)]),
        ];
        let chains = vec![vec![]];
        let bkg_count = vec![vec![0, 0]; 7]; //

        Puzzle {
            blocks,
            chains,
            bkg_count,
        }
    }

    fn solve(&mut self) {
        let mut rng = thread_rng();
        for _ in 0..5000 {
            self.chains = vec![vec![]];
            self.bkg_count = vec![vec![0, 0]; 7]; //
            for (colour, max_num) in BACKGROUNDS.iter() {
                self.bkg_count[*colour as usize][0] = *max_num;
            }
            self.blocks.shuffle(&mut rng);
            self.make_chain(0, true);
        }
    
    }

    fn unscrabble(&mut self, i: usize, orientation: usize, j: Option<usize>) {
        match j {
            Some(chain_num) => {
                self.chains[chain_num].remove(1);
            },
            _ => {
                let last_ch = self.chains.len() - 1;
                self.chains[last_ch].pop();
                if self.chains[last_ch].len() == 0 {
                    self.chains.pop();
                }
            }
        }
        self.blocks[i].location = None;
        self.bkg_count[orientation][1] -= 1;
    }

    fn insert(&mut self, posn: usize, i: usize, orientation: usize, j: Option<usize>) {
        self.blocks[i].location = Some((posn, posn));
        let mut new_grid_posn = GridPosition::new();
        new_grid_posn.block_index = i;
        new_grid_posn.block_orientation = orientation;
        match j {
            Some(chain_num) => {
                self.chains[chain_num].insert(1, new_grid_posn);
            },
            _ => {
                let last_ch = self.chains.len() - 1;
                self.chains[last_ch].push(new_grid_posn);

            }
        }
        self.bkg_count[orientation][1] += 1;
        self.bkg_count[6][1] += 1;
    }

    // recursively ///////////////////////
    fn make_chain(&mut self, posn: usize, new_start: bool) -> (bool, bool) { // first bool if successful, second if line completed
        // if doing n colours the last cube must give equal numbers of each
        if self.bkg_count.iter().take(6).any(|v| v[1] > v[0]) || self.bkg_count[6][1] > 500 {
            return (false, false);
        }

        let mut orientation = posn;
        for i in 0..30 { // try all blocks
            if self.blocks[i].location.is_none() { // only try to insert blocks that haven't been assisgned
                for _ in 0..6 { // try each orientation
                    orientation = (orientation + 1) % 6;
                    //TODO make background colours usize for indexing into blocks NB this will just work with `for orientation in [Blue, Green, Yellow..].iter() {}
                    // then blocks[i].faces[orientation as usize].background
                    if BACKGROUNDS.iter().any(|v| self.blocks[i].faces[orientation].background == v.0) { // check background colours for first three puzzles here
                        // no need to check orientation for chain, just whether this line is started or not.
                        // if line is not started then the Face must be of tp == END, if line *is* started then any Face will do but another END will complete
                        if new_start && self.blocks[i].faces[orientation].tp != END {
                            // see if this will fit into a previous chain
                            for j in 0..self.chains.len() {
                                if self.chains[j].len() == 0 {
                                    continue;
                                }
                                let this_foreground = &self.blocks[i].faces[orientation].foreground; // TODO check if foreground in permitted. NB move up as applies even if not connected
                                let check_foreground = &self.blocks[self.chains[j][0].block_index].faces[self.chains[j][0].block_orientation].foreground;
                                let this_background = &self.blocks[i].faces[orientation].background;
                                // if puzzle type first 3 then background match all chain
                                let check_background = &self.blocks[self.chains[j][0].block_index].faces[self.chains[j][0].block_orientation].background;
                                if this_foreground == check_foreground && this_background == check_background {
                                // if puzzle type 4 then backgrounds all different in chain
                                //if this_foreground == check_foreground && !self.chains[j].iter().any(|v| {&self.blocks[v.block_index].faces[v.block_orientation].background == this_background}) {
                                    self.insert(posn, i, orientation, Some(j));

                                    let (add_ok, complete) = self.make_chain(posn + 1, true); // this will only be true if every recursive call after here was true
                                    if add_ok {
                                        return (true, complete);
                                    } else {
                                        // unscrabble the allocated bock and grid cell
                                        self.unscrabble(i, orientation, Some(j));
                                    }
                                }
                            }
                            continue; // orientation
                        }
                        if !new_start { // only need checks if continuing a string
                            let last_ch = self.chains.len() - 1;
                            let last_cell = self.chains[last_ch].len() - 1; // should be safe if not new_start
                            let check_cell = self.chains[last_ch][last_cell];

                            let this_foreground = &self.blocks[i].faces[orientation].foreground; // TODO check if foreground in permitted. NB move up as applies even if not connected
                            let check_foreground = &self.blocks[check_cell.block_index].faces[check_cell.block_orientation].foreground;
                            if this_foreground != check_foreground {
                                continue;
                            }
                            let this_background = &self.blocks[i].faces[orientation].background;
                            // if puzzle type 1,2,3
                            let check_background = &self.blocks[check_cell.block_index].faces[check_cell.block_orientation].background;
                            if this_background != check_background {
                            // if puzzle type 4
                            //if self.chains[last_ch].iter().any(|v| {&self.blocks[v.block_index].faces[v.block_orientation].background == this_background}) {
                                continue;
                            }
                        }
                        // only get here after checking all sides successfully, this block can go here!
                        // unless last block and this doesn't end a chain
                        if posn == *LAST && (new_start || self.blocks[i].faces[orientation].tp != END) {
                            continue;
                        }
                        if new_start {
                            self.chains.push(vec![]);
                        }
                        self.insert(posn, i, orientation, None);
                        let chain_end = if !new_start && self.blocks[i].faces[orientation].tp == END {
                            true
                        } else {
                            false
                        };
                        if posn == *LAST {
                            // if n colour check number of backgrounds used
                            if self.bkg_count.iter().take(6).any(|v| v[1] > v[0]) {
                            //if bkg_count.iter().take(6).max().unwrap() > &HALF {
                                // unscrabble the allocated bock and grid cell
                                self.unscrabble(i, orientation, None);
                                return (false, false);
                            }
                            for chain in &self.chains {
                                print!(" <--> ");
                                for col in chain {
                                    print!("{:2}{:1}   ", self.blocks[col.block_index].name, as_colour(col.block_orientation));
                                }
                            }
                            print!("\n.\n");
                            return (true, true);
                        }
                        let (add_ok, complete) = self.make_chain(posn + 1, chain_end); // this will only be true if every recursive call after here was true
                        if add_ok {
                            return (true, complete);
                        } else {
                            // unscrabble the allocated bock and grid cell
                            self.unscrabble(i, orientation, None);

                        }
                    }
                }
            }
        }
        (false, false)
    }
}

///////////////////////////////////// main
fn main() {
    let mut rng = thread_rng();
    let mut puzzle = Puzzle::new();
    puzzle.solve();
}