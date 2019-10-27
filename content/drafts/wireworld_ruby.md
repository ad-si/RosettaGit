+++
title = "Wireworld/Ruby"
description = ""
date = 2014-06-23T07:14:26Z
aliases = []
[extra]
id = 4911
[taxonomies]
categories = []
tags = []
+++


```ruby
class WireWorld
  EMPTY     = ' '
  HEAD      = 'H'
  TAIL      = 't'
  CONDUCTOR = '.'
  NEIGHBOURS = [-1,0,1].product([-1,0,1]) - [0,0]
  
  def initialize(string)
    @grid = string.each_line.collect do |line|
              line.chomp.each_char.collect do |char| 
                case char
                when EMPTY, HEAD, TAIL, CONDUCTOR
                  char
                else
                  EMPTY
                end 
              end
            end
    @width = @grid.collect{|row| row.length}.max + 1
    @height = @grid.length
    pad_grid
    @original_grid = Marshal.restore(Marshal.dump @grid)   # this is a deep copy
  end
  
  # initialize from a file
  def self.open(filename)
    self.new(File.read(filename))
  end
  
  def reset
    @grid = @original_grid
  end
  
  # ensure all rows are the same length by padding short rows with empty cells
  def pad_grid
    @grid << []
    @grid.each do |row|
      row.concat(Array.new(@width - row.length, EMPTY))
    end
  end
  
  # the "to_string" method
  def to_s
    @grid.collect {|row| row.join}.join("\n")
  end
  
  # transition all cells simultaneously
  def transition
    @grid = @grid.each_with_index.collect do |row, y| 
              row.each_with_index.collect do |state, x| 
                transition_cell(state, x, y)
              end
            end
  end
  
  # how to transition a single cell
  def transition_cell(current, x, y)
    case current
    when EMPTY then EMPTY
    when HEAD  then TAIL
    when TAIL  then CONDUCTOR
    else neighbours_with_state(x, y).between?(1,2) ? HEAD : CONDUCTOR
    end
  end
  
  # given a position in the grid, find the neighbour cells with a particular state
  def neighbours_with_state(x, y)
    NEIGHBOURS.count {|dx, dy| @grid[y+dy][x+dx] == HEAD}
  end
  
  # run a simulation up to a limit of transitions, or until a recurring
  # pattern is found
  # This will print text to the console
  def run(iterations = 25)
    seen = {}
    for count in 0..iterations
      puts "Generation : #{count}"
      puts to_s
      
      if seen[@grid]
        puts "I've seen this grid before... after #{count} iterations"
        return
      end
      
      seen[@grid] = count
      transition
    end
    puts "ran through #{iterations} iterations"
  end
end
```


Test (Text version)

```ruby
# this is the "2 Clock generators and an XOR gate" example from the wikipedia page
text = <<WORLD
 ......tH
.        ......
 ...Ht...      .
              ....
              .  .....
              ....
 tH......      .
.        ......
 ...Ht...
WORLD

ww = WireWorld.new text
 
ww.run
puts 'bye'
```

{{out}}
<pre style="height:64ex;overflow:scroll">
Generation : 0
 ......tH              
.        ......        
 ...Ht...      .       
              ....     
              .  ..... 
              ....     
 tH......      .       
.        ......        
 ...Ht...              
                       
Generation : 1
 .......t              
.        H.....        
 ..Ht....      .       
              ....     
              .  ..... 
              ....     
 .tH.....      .       
.        ......        
 ..Ht....              
                       
Generation : 2
 ........              
.        tH....        
 .Ht....H      .       
              ....     
              .  ..... 
              ....     
 ..tH....      .       
.        ......        
 .Ht.....              
                       
Generation : 3
 ........              
.        .tH...        
 Ht....Ht      .       
              ....     
              .  ..... 
              ....     
 ...tH...      .       
.        ......        
 Ht......              
                       
Generation : 4
 ........              
H        ..tH..        
 t....Ht.      .       
              ....     
              .  ..... 
              ....     
 ....tH..      .       
H        ......        
 t.......              
                       
Generation : 5
 H.......              
t        ...tH.        
 ....Ht..      .       
              ....     
              .  ..... 
              ....     
 H....tH.      .       
t        ......        
 ........              
                       
Generation : 6
 tH......              
.        ....tH        
 ...Ht...      .       
              ....     
              .  ..... 
              ....     
 tH....tH      .       
.        ......        
 ........              
                       
Generation : 7
 .tH.....              
.        .....t        
 ..Ht....      H       
              ....     
              .  ..... 
              ....     
 .tH....t      .       
.        H.....        
 ........              
                       
Generation : 8
 ..tH....              
.        ......        
 .Ht.....      t       
              HHH.     
              .  ..... 
              ....     
 ..tH....      .       
.        tH....        
 .......H              
                       
Generation : 9
 ...tH...              
.        ......        
 Ht......      .       
              tttH     
              H  H.... 
              ....     
 ...tH...      .       
.        .tH...        
 ......Ht              
                       
Generation : 10
 ....tH..              
H        ......        
 t.......      .       
              ...t     
              t  tH... 
              HHHH     
 ....tH..      .       
.        ..tH..        
 .....Ht.              
                       
Generation : 11
 H....tH.              
t        ......        
 ........      .       
              ....     
              .  .tH.. 
              tttt     
 .....tH.      .       
.        ...tH.        
 ....Ht..              
                       
Generation : 12
 tH....tH              
.        ......        
 ........      .       
              ....     
              .  ..tH. 
              ....     
 ......tH      .       
.        ....tH        
 ...Ht...              
                       
Generation : 13
 .tH....t              
.        H.....        
 ........      .       
              ....     
              .  ...tH 
              ....     
 .......t      H       
.        H....t        
 ..Ht....              
                       
Generation : 14
 ..tH....              
.        tH....        
 .......H      .       
              ....     
              .  ....t 
              HHH.     
 ........      t       
.        tH....        
 .Ht....H              
                       
Generation : 15
 ...tH...              
.        .tH...        
 ......Ht      .       
              ....     
              H  H.... 
              tttH     
 ........      .       
.        .tH...        
 Ht....Ht              
                       
Generation : 16
 ....tH..              
.        ..tH..        
 .....Ht.      .       
              HHHH     
              t  tH... 
              ...t     
 ........      .       
H        ..tH..        
 t....Ht.              
                       
Generation : 17
 .....tH.              
.        ...tH.        
 ....Ht..      .       
              tttt     
              .  .tH.. 
              ....     
 H.......      .       
t        ...tH.        
 ....Ht..              
                       
Generation : 18
 ......tH              
.        ....tH        
 ...Ht...      .       
              ....     
              .  ..tH. 
              ....     
 tH......      .       
.        ....tH        
 ...Ht...              
                       
Generation : 19
 .......t              
.        H....t        
 ..Ht....      H       
              ....     
              .  ...tH 
              ....     
 .tH.....      H       
.        .....t        
 ..Ht....              
                       
Generation : 20
 ........              
.        tH....        
 .Ht....H      t       
              HHH.     
              .  ....t 
              HHH.     
 ..tH....      t       
.        ......        
 .Ht.....              
                       
Generation : 21
 ........              
.        .tH...        
 Ht....Ht      .       
              tttH     
              .  H.... 
              tttH     
 ...tH...      .       
.        ......        
 Ht......              
                       
Generation : 22
 ........              
H        ..tH..        
 t....Ht.      .       
              ...t     
              .  t.... 
              ...t     
 ....tH..      .       
H        ......        
 t.......              
                       
Generation : 23
 H.......              
t        ...tH.        
 ....Ht..      .       
              ....     
              .  ..... 
              ....     
 H....tH.      .       
t        ......        
 ........              
                       
I've seen this grid before... after 23 iterations
bye

```


The GUI version
{{libheader|Ruby/Tk}}

The GUI is somewhat "halfway", in that it animates a text widget so it's not "real" graphics.

```ruby
require 'tk'
 
class WireWorld
  def run_tk
    @tk_root = TkRoot.new(title: "WireWorld")
    
    @tk_text = TkText.new(width: @width, height: @height, font: 'courier')
    @tk_text.insert('end', self.to_s).state('disabled')
    
    @tk_after_interval = 150
    faster_cmd = proc {@tk_after_interval = [25, @tk_after_interval-25].max}
    slower_cmd = proc {@tk_after_interval += 25}
    reset_cmd = proc {self.reset}
    close_cmd = proc do
      @tk_root.after_cancel(@tk_after_id)
      @tk_root.destroy
    end
    
    controls = TkFrame.new
    [ TkButton.new(controls, text: 'Slower', command: slower_cmd),
      TkButton.new(controls, text: 'Faster', command: faster_cmd),
      TkButton.new(controls, text: 'Reset',  command: reset_cmd),
      TkButton.new(controls, text: 'Close',  command: close_cmd),
    ].each {|btn| btn.pack(expand: 1, fill: 'x', side: 'left')}
    
    @tk_text.pack(expand: 1, fill: 'both')
    controls.pack(fill: 'x')
    
    @tk_after_id = @tk_root.after(500) {animate}
    Tk.mainloop
  end
  
  def animate
    transition 
    @tk_text.state('normal') \
            .delete('1.0','end') \
            .insert('end', self.to_s) \
            .state('disabled')
    @tk_after_id = @tk_root.after(@tk_after_interval) {animate}
  end
end

ww = WireWorld.new text
ww.run_tk
```


{{libheader|Shoes}}

```ruby
ww = WireWorld.new text
Shoes.app(title: "Wireworld") do
  world = para('', family: 'monospace')
  animate(4) do
    world.text = ww.to_s
    ww.transition
  end
end
```

