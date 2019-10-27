+++
title = "RCRPG/Java"
description = ""
date = 2014-06-11T17:11:08Z
aliases = []
[extra]
id = 17701
[taxonomies]
categories = []
tags = []
+++

{{collection|RCRPG}}
Translation of Python via D.

```java5
import java.lang.reflect.Method;
import java.util.*;

public class RCRPG {

    HashMap<String, Direction> directions;
    HashMap<String, List<String>> aliases;
    HashMap<List<Integer>, String> roomNames;

    private RCRPG() {
        directions = new HashMap<>();
        directions.put("north", new Direction("north", "south", 0, -1, 0));
        directions.put("east", new Direction("east", "west", 1, 0, 0));
        directions.put("south", new Direction("south", "north", 0, 1, 0));
        directions.put("west", new Direction("west", "east", -1, 0, 0));
        directions.put("up", new Direction("up", "down", 0, 0, 1));
        directions.put("down", new Direction("down", "up", 0, 0, -1));

        aliases = new HashMap<>();
        for (String k : directions.keySet()) {
            String[] a = {"move", k};
            aliases.put(k, new ArrayList<>(Arrays.asList(a)));
        }

        roomNames = new HashMap<>();
        roomNames.put(Arrays.asList(0, 0, 0), "the starting room");
        roomNames.put(Arrays.asList(1, 1, 5), "the prize room");
    }

    class Direction {
        final String name;
        final List<Integer> coords;
        final String opposite;

        public Direction(String n, String o, List<Integer> c) {
            name = n;
            opposite = o;
            coords = c;
        }

        public Direction(String n, String o, int... c) {
            this(n, o, Arrays.asList(c[0], c[1], c[2]));
        }

        List<Integer> getNewCoords(List<Integer> c) {
            List<Integer> result = new ArrayList<>(3);
            for (int i = 0; i < 3; i++)
                result.add(coords.get(i) + c.get(i));
            return result;
        }
    }

    class Room {
        Set<String> items;
        Set<String> passages;
        String[] allItems = {"sledge", "ladder", "gold"};

        Room(String item) {
            items = new HashSet<>();
            if (!item.isEmpty())
                items.add(item);
            else {
                int idx = (int) (Math.random() * 4);
                if (idx > 0)
                    items.add(allItems[idx - 1]);
            }
            passages = new HashSet<>();
        }

        String describe(List<Integer> location) {
            StringBuilder sb = new StringBuilder("You are at: ");

            String roomName = roomNames.get(location);
            if (roomName != null)
                sb.append(roomName);
            else
                sb.append(Arrays.toString(location.toArray()));

            if (!items.isEmpty()) {
                sb.append("\nOn the ground you can see: ");
                sb.append(Arrays.toString(items.toArray()));
            }

            sb.append("\nExits are: ");
            if (passages.isEmpty()) {
                sb.append("none");
            } else {
                for (String p : passages)
                    sb.append(p).append(" ");
            }

            return sb.toString();
        }

        Set<String> take(String target) {
            Set<String> result = new HashSet<>();
            if (target.equals("all")) {
                result.addAll(items);
                items.clear();
                System.out.println("You now have everything in the room.");
            } else {
                if (items.remove(target)) {
                    result.add(target);
                    System.out.printf("Taken %s.%n", target);
                } else {
                    System.out.println("Item not found.");
                }
            }
            return result;
        }
    }

    class World {
        List<Integer> currPos = Arrays.asList(0, 0, 0);
        Map<List<Integer>, Room> rooms;
        Set<String> inv;
        String equipped = "";

        World() {
            rooms = new HashMap<>();
            inv = new HashSet<>();
            rooms.put(currPos, new Room("sledge"));
        }

        String look() {
            return rooms.get(currPos).describe(currPos);
        }

        void move(String direction) {
            if (direction.equals("up")) {
                if (!rooms.get(currPos).items.contains("ladder")) {
                    System.out.println("There's no ladder in this room");
                    return;
                }
            }

            Direction dir = directions.get(direction);
            if (dir == null) {
                System.out.println("That's not a direction");
            } else if (rooms.get(currPos).passages.contains(dir.name)) {
                currPos = dir.getNewCoords(currPos);
            } else
                System.out.println("You can't go that way.");

        }

        void newalias(String newAlias, String[] command) {
            if (command.length == 2) {
                if (aliases.get(command[0]) != null) {
                    System.out.println("You cannot alias an alias");
                } else {
                    aliases.put(newAlias, Arrays.asList(command));
                    System.out.println("Alias created");
                }
            } else
                System.out.println("Wrong number of arguments for alias");
        }

        void inventory() {
            if (inv.isEmpty())
                System.out.println("You aren't carrying anything.");
            else {
                System.out.print("Carrying: ");
                System.out.println(Arrays.toString(inv.toArray()));
            }
            if (!equipped.isEmpty())
                System.out.println("Holding: " + equipped);
        }

        void take(String target) {
            inv.addAll(rooms.get(currPos).take(target));
        }

        void drop(String target) {
            if (target.equals("all")) {
                rooms.get(currPos).items.addAll(inv);
                inv.clear();
                System.out.println("Everything dropped");
            } else {
                if (inv.remove(target)) {
                    rooms.get(currPos).items.add(target);
                    System.out.println("Dropped " + target + ".");
                } else
                    System.out.println("Could not find item in inventory.");
            }
        }

        void equip(String item) {
            if (inv.remove(item)) {
                if (!equipped.isEmpty())
                    unequip();
                equipped = item;
                System.out.println("Equipped " + item + ".");
            } else
                System.out.println("You aren't carrying that.");
        }

        void unequip() {
            if (equipped.isEmpty())
                System.out.println("You aren't equipped with anything.");
            else {
                inv.add(equipped);
                System.out.println("Unequipped " + equipped + ".");
                equipped = "";
            }
        }

        void name(String roomName) {
            roomNames.put(currPos, roomName);
        }

        void dig(String dir) {
            if (!equipped.equals("sledge")) {
                System.out.println("You don't have a digging tool equipped.");
                return;
            }

            Direction direction = directions.get(dir);
            if (direction == null) {
                System.out.println("That's not a direction.");
                return;
            }

            Set<String> passages = rooms.get(currPos).passages;
            if (!passages.contains(dir)) {
                passages.add(dir);
                List<Integer> newPos = direction.getNewCoords(currPos);
                if (rooms.get(newPos) == null)
                    rooms.put(newPos, new Room(""));
                rooms.get(newPos).passages.add(direction.opposite);
                System.out.println("You've dug a tunnel.");
            } else
                System.out.println("Already a tunnel that way.");
        }
    }

    void play() {
        World world = new World();
        System.out.println("Welcome to the dungeon!\nGrab the sledge && make"
                + " your way to room 1,1,5 for a non-existent prize!");

        Scanner in = new Scanner(System.in);
        List<String> tokens;
        while (true) {
            System.out.printf("%s%n", world.look());
            System.out.print("> ");
            tokens = Arrays.asList(in.nextLine().toLowerCase().split(" "));
            switch (tokens.get(0)) {
                case "quit":
                    System.out.println("Thanks for playing!");
                    return;
                case "alias":
                    tokens.set(0, "newalias");
                    break;
                case "name":
                    StringBuilder sb = new StringBuilder();
                    for (ListIterator li = tokens.listIterator(1); li.hasNext();)
                        sb.append(li.next()).append(" ");
                    tokens = new ArrayList<>(2);
                    tokens.add("name");
                    tokens.add(sb.toString());
                    break;
            }

            while (!tokens.isEmpty()) {
                List<String> a = aliases.get(tokens.get(0));
                if (a != null && !tokens.get(0).equals(a.get(0))) {
                    tokens = new ArrayList(tokens.subList(1, tokens.size()));
                    tokens.addAll(0, a);
                    continue;
                }
                try {
                    processArguments(tokens, world);
                } catch (Exception e) {
                    System.out.println("Invalid command");
                }
                break;
            }
        }
    }

    void processArguments(List<String> args, World w) throws Exception {
        Method m;
        Class c = w.getClass();
        int numArgs = args.size();
        if (numArgs == 1) {
            m = c.getDeclaredMethod(args.get(0));
            m.invoke(w);
        } else if (numArgs == 2) {
            m = c.getDeclaredMethod(args.get(0), String.class);
            m.invoke(w, args.get(1));
        } else {
            String[] a = args.subList(2, args.size()).toArray(new String[]{});
            m = c.getDeclaredMethod(args.get(0), String.class, String[].class);
            m.invoke(w, args.get(1), a);
        }
    }

    public static void main(String[] args) {
        new RCRPG().play();
    }
}
```

