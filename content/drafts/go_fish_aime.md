+++
title = "Go Fish/Aime"
description = ""
date = 2018-05-08T17:30:25Z
aliases = []
[extra]
id = 14327
[taxonomies]
categories = []
tags = []
+++

{{collection|Go Fish}}
The AI will randomly ask a rank known to be in the human's hand (a card in the AI's hand that the human has asked for before and the AI hasn't asked for before). If there are no known ranks, the AI will ask for the lowest rank it has (the strategy has the virtue of not giving away the player's hand).

```aime
void
shuffle(list l)
{
    integer c, e;

    for (, c in b_draft("SHDC")) {
        for (, e in b_draft("A23456789TJQK")) {
            l.p_text(drand(~l), data().append(e).append(c));
        }
    }
}

void deal(list, text, record, integer &);

void
check_book(list deck, text name, record p, text pick, integer &books)
{
    integer i;
    text s;

    i = 0;
    s = pick;
    while (rsk_greater(p, s, s)) {
        if (s[0] == pick[0]) {
            i += 1;
        } else {
            break;
        }
    }

    if (i == 4) {
        books += 1;
        while (i) {
            i -= 1;
            rsk_greater(p, pick, s);
            p.delete(s);
        }

        if (!~p) {
            deal(deck, name, p, books);
        }
    }
}

void
deal(list deck, text name, record p, integer &p_books)
{
    if (~deck) {
        text s;

        lf_e_text(s, deck);
        o_form("dealing /w8/", name);
        if (!compare(name, "player")) {
            o_(" ", s);
        }
        o_text("\n");
        p[s] = 0;
        check_book(deck, name, p, delete(s, 1), p_books);
    }
}

void
show(text player, integer books, record p)
{
    text s;

    o_form("~: /w2/ books", player, books);

    if (!compare(player, "P")) {
        for (s in p) {
            o_(" ", s);
        }
    } else {
        call_n(~p, o_text, " **");
    }

    o_newline();
}

integer
draw(record p, text s, text &d)
{
    integer h;

    h = rsk_greater(p, s, d);
    if (h) {
        if (s[0] != d[0]) {
            h = 0;
        } else {
            p.delete(d);
        }
    }

    return h;
}

integer
user(list deck, record e, integer &c_books, record u, integer &u_books,
     record asked, file input)
{
    integer t;
    text pick, s;

    while (1) {
        o_text("your pick?\n");
        f_line(input, pick);
        if (~pick != 1 || place("A23456789TJQK", pick[0]) == -1) {
            o_text("bad choice\n");
        } else {
            rsk_greater(u, pick, s);
            if (s[0] != pick[0]) {
                o_("you don't have any ", pick, "s\n");
            } else {
                break;
            }
        }
    }

    asked.a_integer(pick, 0);

    t = draw(e, pick, s);
    if (!t) {
        o_("computer has no ", pick, "s\n");
        deal(deck, "player", u, u_books);
    } else {
        do {
            u[s] = 0;
        } while (draw(e, pick, s));
        if (!~e) {
            deal(deck, "computer", e, c_books);
        }

        check_book(deck, "player", u, pick, u_books);
    }

    return t;
}

void
mull(record c, record asked, text &pick)
{
    list l;
    text s;

    for (s in asked) {
        text t;

        if (rsk_greater(c, s, t)) {
            if (s[0] == t[0]) {
                l.append(s);
            }
        }
    }

    if (~l) {
        pick = l[drand(~l - 1)];
    }
}

integer
fnet(list deck, record e, integer &c_books, record u, integer &u_books,
     record asked)
{
    integer t;
    text pick, s;

    e.first(pick);
    # seemingly dumb choice: ask the first book
    pick = delete(pick, 1);
    # refine choice to something the user has asked for
    mull(e, asked, pick);
    o_("computer asks ", pick, "s\n");
    t = !draw(u, pick, s);
    asked.resign(pick);
    if (t) {
        deal(deck, "computer", e, c_books);
    } else {
        do {
            e[s] = 0;
        } while (draw(u, pick, s));
        if (!~u) {
            deal(deck, "player", u, u_books);
        }

        check_book(deck, "computer", e, pick, c_books);
    }

    return t;
}

integer
main(void)
{
    integer c_books, u_books;
    list deck;
    file in;
    integer i, u_turn;
    record ai, e, u;

    c_books = u_books = 0;
    u_turn = 1;

    in.stdin;

    deck.shuffle;

    i = 9;
    while (i) {
        deal(deck, "player", u, u_books);
        deal(deck, "computer", e, c_books);
        i -= 1;
    }

    while (c_books + u_books != 13) {
        show("C", c_books, e);
        show("P", u_books, u);
        if (u_turn) {
            u_turn = user(deck, e, c_books, u, u_books, ai, in);
        } else {
            u_turn = fnet(deck, e, c_books, u, u_books, ai);
        }
    }

    o_("Computer has ", c_books, " books.\n");
    o_("Player has ", u_books, " books.\n");
    o_(c_books < u_books ? "Player" : "Computer", " wins.\n");

    return 0;
}

```

