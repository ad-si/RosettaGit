+++
title = "Hunt The Wumpus/Javascript"
description = ""
date = 2017-07-16T06:59:02Z
aliases = []
[extra]
id = 19107
[taxonomies]
categories = []
tags = []
+++

{{collection|Hunt_The_Wumpus}}
[[Javascript]] (node) version of [[:Category:Hunt_The_Wumpus|Hunt The Wumpus]] .

==Code==

```javascript


const _ = require('lodash'),

      ROOMS         = 20,
      ROOMS_TUNNELS = 3,
      BATS          = 2,
      PITS          = 2,
      ARROWS        = 5,

      HELP = `
Welcome to "Hunt the Wumpus"
The wumpus lives in a cave of 20 rooms. Each room has 3 tunnels to
other rooms. (Look at a dodecahedron to see how this works. If you
dont know what a dodecahedron is, ask someone.)
Hazards:
 Bottomless pits - Two rooms have bottomless pits in them. If you go
   there, you fall into the pit (& lose)!
 Super bats - Two other rooms have super bats. If you go there, a
   bat grabs you and takes you to some other room at random (which
   may be troublesome).
Wumpus:
   The wumpus is not bothered by hazards. (He has sucker feet and is
   too big for a bat to lift.)  Usually he is asleep. Two things
   wake him up: your shooting an arrow, or your entering his room.
   If the wumpus wakes, he moves one room or stays still.
   After that, if he is where you are, he eats you up and you lose!
You:
   Each turn you may move or shoot a crooked arrow.
   Moving:  You can move one room (through one tunnel).
   Arrows:  You have 5 arrows.  You lose when you run out.
      You can only shoot to nearby rooms.
      If the arrow hits the wumpus, you win.
Warnings:
   When you are one room away from a wumpus or hazard, the computer
   says:
   Wumpus:  "You smell something terrible nearby."
   Bat   :  "You hear a rustling."
   Pit   :  "You feel a cold wind blowing from a nearby cavern."
`;


///////////////////
// LODASH IMPORT //
///////////////////

// import all lodash functions to the main namespace, but isNaN not to cause conflicts
_.each(_.keys(_), k => global[k === 'isNaN' ? '_isNaN' : k] = _[k]);

///////////
// WORLD //
///////////

// create a cave of 20 interconnected rooms. each room is linked to 3 other rooms
const cave = chain(range(ROOMS))
              .map(n => [n, [], []])
              .reduce((acc, n, idx, lst) => {
                acc = acc || lst;

                // possible candidates: not self; not already linked to the current room;
                //   not already having three tunnels
                const candidates = reject(acc, nn => first(nn) === first(n)
                        || contains(nn[1], first(n))
                        || contains(n[1], first(nn))
                        || nn[1].length === ROOMS_TUNNELS),
                      chosen = sample(candidates, ROOMS_TUNNELS - n[1].length);

                each(chosen, c => {
                  n[1].push(first(c));
                  c[1].push(first(n));
                })

                return acc;
              }, null)
              .thru(c => {
                const findEmptyAndAdd = (element) => sample(filter(c, r => isEmpty(r[2])))[2] = [element];

                findEmptyAndAdd('wumpus');
                times(BATS, partial(findEmptyAndAdd, 'bat'));
                times(PITS, partial(findEmptyAndAdd, 'pit'));

                return c;
              })
              .value();

let world = {
  cave: cave,
  player: {
    room:  sample(filter(cave, r => isEmpty(r[2]))),
    arrows: ARROWS
  }
};

// graphviz visualization:
// const graphvizString = 'graph {\n' + map(cave, n => `    ${ first(n) } -- {${ n[1].join(', ') }};`).join('\n') + '\n}';

/////////////
// HELPERS //
/////////////

function isNearby(world, element) {
  return chain(world.player.room[1])
          .map(r => find(world.cave, rr => first(rr) === r))
          .any(r => contains(r[2], element))
          .value();
}

function isInRoom (world, element, room) {
  room = room || world.player.room;
  return contains(room[2], element);
}

function roomIsNearby(world, roomId) {
  return contains(world.player.room[1].concat(first(world.player.room)), roomId);
}

function roomById (world, roomId) {
  return find(world.cave, r => first(r) === roomId);
}

function randomEmptyRoom(world) {
  return sample(filter(world.cave, r => isEmpty(r[2])));
}

///////////////////////
// WORLD INTERACTION //
///////////////////////

function describeCurrentRoom(world) {
  const pit    = isNearby(world, 'pit')    ? '\nYou feel a cold wind blowing from a nearby cavern.' : '',
        wumpus = isNearby(world, 'wumpus') ? '\nYou smell something terrible nearby.' : '',
        bat    = isNearby(world, 'bat')    ? '\nYou hear a rustling.' : '';

  return `You are in room ${ first(world.player.room) }
Exits go to: ${ world.player.room[1].join(', ') }${ pit }${ wumpus }${ bat }`;
}

function processInput (world, input) {

  function validateRoom() {
    let roomId;

    if (!_isNaN(roomId = parseInt(input, 10)) && roomId < ROOMS && roomIsNearby(world, roomId)) {
      return roomById(world, roomId);
    }
  }

  // quit
  if (processInput.awaiting === 'quit') {
    if (input !== 'y' && input !== 'n') {
      console.log("That doesn't make any sense");
    } else {
      if (input === 'y') {
        console.log('Goodbye, braveheart!');
        process.exit(0);
      } else {
        console.log('Good. the Wumpus is looking for you!');
      }
    }

    processInput.awaiting = null;
    return world;
  }

  if (input === 'q') {
    console.log("Are you so easily scared? [y/n]");
    processInput.awaiting = 'quit';
    return world;
  }

  // move
  if (processInput.awaiting === 'move') {
    let room;

    if (!(room = validateRoom())) {
      console.log('There are no tunnels from here to that room');
    } else {
      world.player.room = room;
      if (isInRoom(world, 'wumpus')) {
        console.log('The wumpus ate you up!\nGAME OVER');
        process.exit(0);
      }
      if (isInRoom(world, 'pit')) {
        console.log('You fall into a bottomless pit!\nGAME OVER');
        process.exit(0);
      }
      if (isInRoom(world, 'bat')) {
        world.player.room = randomEmptyRoom(world);
        console.log('The bats whisk you away!');
      }

      console.log(describeCurrentRoom(world));
    }
    console.log('What do you want to do? (m)ove or (s)hoot?');

    processInput.awaiting = null;
    return world;
  }

  if (input === 'm') {
    processInput.awaiting = 'move';
    console.log('Where?');
    return world;
  }

  // shoot
  if (processInput.awaiting === 'shoot') {
    let room;

    if (!(room = validateRoom())) {
      console.log('There are no tunnels from here to that room');
    } else {
      if (isInRoom(world, 'wumpus', room)) {
        console.log("YOU KILLED THE WUMPUS! GOOD JOB, BUDDY!!!");
        process.exit(0);
      } else {
        if (random(3) > 0) {
          let newWumpusRoom = randomEmptyRoom(world);

          find(world.cave, partial(isInRoom, world, 'wumpus'))[2] = [];
          newWumpusRoom[2] = ['wumpus'];

          if (isEqual(world.player.room, newWumpusRoom)) {
            console.log('You woke up the wumpus and he ate you!\nGAME OVER');
            process.exit(0);
          } else {
            console.log('You heard a rumbling in a nearby cavern.');
          }
        }
      }
    }

    world.player.arrows--;
    if (world.player.arrows === 0) {
      console.log('You ran out of arrows.\nGAME OVER');
      process.exit(0);
    }

    processInput.awaiting = null;
    return world;
  }

  if (input === 's') {
    processInput.awaiting = 'shoot';
    console.log('Where?');
    return world;
  }

  if (input === 'h') {
    console.log(HELP);
    return world;
  }

  console.log("That doesn't make any sense");
  return world;
}
processInput.awaiting = 'move';

///////////////
// MAIN LOOP //
///////////////

process.stdin.resume();
process.stdin.setEncoding('utf8');

process.stdin.on('data', input => {
  world = processInput(world, trim(input));
  console.log('------------------------------');
});

console.log(HELP);

// trigger the initial input. since processInput.awaiting is set to "move",
//  sending the room where the player is will actually trigger the
//  description of the room
process.stdin.emit('data', first(world.player.room));

```

