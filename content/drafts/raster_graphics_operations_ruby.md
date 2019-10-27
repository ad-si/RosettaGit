+++
title = "Raster graphics operations/Ruby"
description = ""
date = 2014-09-08T15:33:54Z
aliases = []
[extra]
id = 4835
[taxonomies]
categories = []
tags = []
+++

[[Category:Ruby]]
==The Code==
Collecting all the Ruby code from [[:Category:Raster graphics operations]], so one can invoke: <code>require 'raster_graphics'</code>

Uses the [https://github.com/wvanbergen/chunky_png ChunkyPNG] pure-Ruby PNG library.


```ruby
###########################################################################
# Represents an RGB[http://en.wikipedia.org/wiki/Rgb] colour.  
class RGBColour
  # Red, green and blue values must fall in the range 0..255.
  def initialize(red, green, blue)
    ok = [red, green, blue].inject(true) {|ok,c| ok &= c.between?(0,255)}
    unless ok
      raise ArgumentError, "invalid RGB parameters: #{[red, green, blue].inspect}"
    end
    @red, @green, @blue = red, green, blue
  end
  attr_reader :red, :green, :blue
  alias_method :r, :red
  alias_method :g, :green
  alias_method :b, :blue

  # Return the list of [red, green, blue] values.
  #     RGBColour.new(100,150,200).values # => [100, 150, 200]
  # call-seq:
  # values -> array
  #
  def values
    [@red, @green, @blue]
  end

  # Equality test: two RGBColour objects are equal if they have the same
  # red, green and blue values.
  # call-seq:
  #     ==(a_colour) -> true or false
  #
  def ==(a_colour)
    values == a_colour.values
  end

  # Comparison test: compares two RGBColour objects based on their #luminosity value
  # call-seq:
  #     <=>(a_colour) -> -1, 0, +1
  #
  def <=>(a_colour)
    self.luminosity <=> a_colour.luminosity
  end

  # Calculate a integer luminosity value, in the range 0..255
  #     RGBColour.new(100,150,200).luminosity # => 142
  # call-seq:
  #     luminosity -> int
  #
  def luminosity
    Integer(0.2126*@red + 0.7152*@green + 0.0722*@blue)
  end

  # Return a new RGBColour value where all the red, green, blue values are the
  # #luminosity value.
  #     RGBColour.new(100,150,200).to_grayscale.values # => [142, 142, 142]
  # call-seq:
  #     to_grayscale -> a_colour
  #
  def to_grayscale
    l = luminosity
    self.class.new(l, l, l)
  end

  # Return a new RGBColour object given an iteration value for the Pixmap.mandelbrot
  # method.
  def self.mandel_colour(i)
    self.new( 16*(i % 15), 32*(i % 7), 8*(i % 31) )
  end

  RED   = RGBColour.new(255,0,0)
  GREEN = RGBColour.new(0,255,0)
  BLUE  = RGBColour.new(0,0,255)
  YELLOW= RGBColour.new(255,255,0)
  BLACK = RGBColour.new(0,0,0)
  WHITE = RGBColour.new(255,255,255)
end

###########################################################################
# A Pixel represents an (x,y) point in a Pixmap.
Pixel = Struct.new(:x, :y)

###########################################################################
class Pixmap
  def initialize(width, height)
    @width = width
    @height = height
    @data = fill(RGBColour::WHITE)
  end
  attr_reader :width, :height

  def fill(colour)
    @data = Array.new(@width) {Array.new(@height, colour)}
  end

  def validate_pixel(x,y)
    unless x.between?(0, @width-1) and y.between?(0, @height-1)
      raise ArgumentError, "requested pixel (#{x}, #{y}) is outside dimensions of this bitmap"
    end
  end

  ###############################################
  def [](x,y)
    validate_pixel(x,y)
    @data[x][y]
  end
  alias_method :get_pixel, :[]

  def []=(x,y,colour)
    validate_pixel(x,y)
    @data[x][y] = colour
  end
  alias_method :set_pixel, :[]=

  def each_pixel
    if block_given?
      @height.times {|y| @width.times {|x| yield x,y}}
    else
      to_enum(:each_pixel)
    end
  end

  ###############################################
  # write to file/stream
  PIXMAP_FORMATS = ["P3", "P6"]   # implemented output formats
  PIXMAP_BINARY_FORMATS = ["P6"]  # implemented output formats which are binary

  def write_ppm(ios, format="P6")
    if not PIXMAP_FORMATS.include?(format)
      raise NotImplementedError, "pixmap format #{format} has not been implemented" 
    end
    ios.puts format, "#{@width} #{@height}", "255"
    ios.binmode if PIXMAP_BINARY_FORMATS.include?(format)
    each_pixel do |x, y|
      case format
      when "P3" then ios.print @data[x][y].values.join(" "),"\n"
      when "P6" then ios.print @data[x][y].values.pack('C3')
      end
    end
  end

  def save(filename, opts={:format=>"P6"})
    File.open(filename, 'w') do |f|
      write_ppm(f, opts[:format])
    end
  end
  alias_method :write, :save

  def print(opts={:format=>"P6"})
    write_ppm($stdout, opts[:format])
  end

  def save_as_jpeg(filename, quality=75)
    # using the ImageMagick convert tool
    begin
      pipe = IO.popen("convert ppm:- -quality #{quality} jpg:#{filename}", 'w')
      write_ppm(pipe)
    rescue SystemCallError => e
      warn "problem writing data to 'convert' utility -- does it exist in your $PATH?"
    ensure
      pipe.close rescue false
    end
  end

  def save_as_png(filename)
    require 'chunky_png'
    stream = StringIO.new("", "r+")
    each_pixel {|x, y| stream << self[x, y].values.pack("ccc")}
    stream.seek(0)
    ChunkyPNG::Canvas.extend(ChunkyPNG::Canvas::StreamImporting)
    canvas = ChunkyPNG::Canvas.from_rgb_stream(width, height, stream)
    canvas.to_image.save(filename)
  end

  ###############################################
  # read from file/pipe
  def self.read_ppm(ios)
    format = ios.gets.chomp
    width, height = ios.gets.chomp.split.map {|n| n.to_i }
    max_colour = ios.gets.chomp

    if (not PIXMAP_FORMATS.include?(format)) or 
        width < 1 or height < 1 or
        max_colour != '255'
    then
      ios.close
      raise StandardError, "file '#{filename}' does not start with the expected header"
    end
    ios.binmode if PIXMAP_BINARY_FORMATS.include?(format)

    bitmap = self.new(width, height)
    bitmap.each_pixel do |x,y|
      # read 3 bytes
      red, green, blue = case format
        when 'P3' then ios.gets.chomp.split
        when 'P6' then ios.read(3).unpack('C3')
      end
      bitmap[x,y] = RGBColour.new(red, green, blue)
    end
    ios.close
    bitmap
  end

  def self.open(filename)
    read_ppm(File.open(filename, 'r'))
  end

  def self.open_from_jpeg(filename)
    unless File.readable?(filename)
      raise ArgumentError, "#{filename} does not exists or is not readable."
    end
    begin
      pipe = IO.popen("convert jpg:#{filename} ppm:-", 'r')
      read_ppm(pipe)
    rescue SystemCallError => e
      warn "problem reading data from 'convert' utility -- does it exist in your $PATH?"
    ensure
      pipe.close rescue false
    end
  end

  ###############################################
  # conversion methods
  def to_grayscale
    gray = self.class.new(@width, @height)
    each_pixel do |x,y|
      gray[x,y] = self[x,y].to_grayscale
    end
    gray
  end

  def to_blackandwhite
    hist = histogram

    # find the median luminosity
    median = nil
    sum = 0
    hist.keys.sort.each do |lum|
      sum += hist[lum]
      if sum > @height * @width / 2
        median = lum
        break
      end
    end

    # create the black and white image
    bw = self.class.new(@width, @height)
    each_pixel do |x,y|
      bw[x,y] = self[x,y].luminosity < median ? RGBColour::BLACK : RGBColour::WHITE
    end
    bw
  end

  def save_as_blackandwhite(filename)
    to_blackandwhite.save(filename)
  end

  ###############################################
  def draw_line(p1, p2, colour)
    validate_pixel(p1.x, p2.y)
    validate_pixel(p2.x, p2.y)

    x1, y1 = p1.x, p1.y
    x2, y2 = p2.x, p2.y
 
    steep = (y2 - y1).abs > (x2 - x1).abs
    if steep
      x1, y1 = y1, x1
      x2, y2 = y2, x2
    end
    if x1 > x2
      x1, x2 = x2, x1
      y1, y2 = y2, y1
    end

    deltax = x2 - x1
    deltay = (y2 - y1).abs
    error = deltax / 2
    ystep = y1 < y2 ? 1 : -1
 
    y = y1
    x1.upto(x2) do |x|
      pixel = steep ? [y,x] : [x,y]
      self[*pixel] = colour
      error -= deltay
      if error < 0
        y += ystep
        error += deltax
      end
    end
  end

  ###############################################
  def draw_line_antialised(p1, p2, colour)
    x1, y1 = p1.x, p1.y
    x2, y2 = p2.x, p2.y
 
    steep = (y2 - y1).abs > (x2 - x1).abs
    if steep
      x1, y1 = y1, x1
      x2, y2 = y2, x2
    end
    if x1 > x2
      x1, x2 = x2, x1
      y1, y2 = y2, y1
    end
    deltax = x2 - x1
    deltay = (y2 - y1).abs
    gradient = 1.0 * deltay / deltax
 
    # handle the first endpoint
    xend = x1.round
    yend = y1 + gradient * (xend - x1)
    xgap = (x1 + 0.5).rfpart
    xpxl1 = xend
    ypxl1 = yend.truncate
    put_colour(xpxl1, ypxl1, colour, steep, yend.rfpart * xgap)
    put_colour(xpxl1, ypxl1 + 1, colour, steep, yend.fpart * xgap)
    itery = yend + gradient
 
    # handle the second endpoint
    xend = x2.round
    yend = y2 + gradient * (xend - x2)
    xgap = (x2 + 0.5).rfpart
    xpxl2 = xend
    ypxl2 = yend.truncate
    put_colour(xpxl2, ypxl2, colour, steep, yend.rfpart * xgap)
    put_colour(xpxl2, ypxl2 + 1, colour, steep, yend.fpart * xgap)
 
    # in between
    (xpxl1 + 1).upto(xpxl2 - 1).each do |x|
      put_colour(x, itery.truncate, colour, steep, itery.rfpart)
      put_colour(x, itery.truncate + 1, colour, steep, itery.fpart)
      itery = itery + gradient
    end
  end

  def put_colour(x, y, colour, steep, c)
    x, y = y, x if steep
    self[x, y] = anti_alias(colour, self[x, y], c)
  end

  def anti_alias(new, old, ratio)
    blended = new.values.zip(old.values).map {|n, o| (n*ratio + o*(1.0 - ratio)).round}
    RGBColour.new(*blended)
  end

  ###############################################
  def draw_circle(pixel, radius, colour)
    validate_pixel(pixel.x, pixel.y)
 
    self[pixel.x, pixel.y + radius] = colour
    self[pixel.x, pixel.y - radius] = colour
    self[pixel.x + radius, pixel.y] = colour
    self[pixel.x - radius, pixel.y] = colour
 
    f = 1 - radius
    ddF_x = 1
    ddF_y = -2 * radius
    x = 0
    y = radius
    while x < y
      if f >= 0
        y -= 1
        ddF_y += 2
        f += ddF_y
      end
      x += 1
      ddF_x += 2
      f += ddF_x
      self[pixel.x + x, pixel.y + y] = colour
      self[pixel.x + x, pixel.y - y] = colour
      self[pixel.x - x, pixel.y + y] = colour
      self[pixel.x - x, pixel.y - y] = colour
      self[pixel.x + y, pixel.y + x] = colour
      self[pixel.x + y, pixel.y - x] = colour
      self[pixel.x - y, pixel.y + x] = colour
      self[pixel.x - y, pixel.y - x] = colour
    end
  end

  ###############################################
  def flood_fill(pixel, new_colour)
    current_colour = self[pixel.x, pixel.y]
    queue = Queue.new
    queue.enqueue(pixel)
    until queue.empty?
      p = queue.dequeue
      if self[p.x, p.y] == current_colour
        west = find_border(p, current_colour, :west)
        east = find_border(p, current_colour, :east)
        draw_line(west, east, new_colour)
        q = west
        while q.x <= east.x
          [:north, :south].each do |direction|
            n = neighbour(q, direction)
            queue.enqueue(n) if self[n.x, n.y] == current_colour
          end
          q = neighbour(q, :east)
        end
      end
    end
  end

  def neighbour(pixel, direction)
    case direction
    when :north then Pixel[pixel.x, pixel.y - 1]
    when :south then Pixel[pixel.x, pixel.y + 1]
    when :east  then Pixel[pixel.x + 1, pixel.y]
    when :west  then Pixel[pixel.x - 1, pixel.y]
    end
  end

  def find_border(pixel, colour, direction)
    nextp = neighbour(pixel, direction)
    while self[nextp.x, nextp.y] == colour
      pixel = nextp
      nextp = neighbour(pixel, direction)
    end
    pixel
  end

  ###############################################
  def median_filter(radius=3)
    if radius.even?
      radius += 1
    end
    filtered = self.class.new(@width, @height)


    $stdout.puts "processing #{@height} rows"
    pb = ProgressBar.new(@height) if $DEBUG

    @height.times do |y|
      @width.times do |x|
        window = []
        (x - radius).upto(x + radius).each do |win_x|
          (y - radius).upto(y + radius).each do |win_y|
            win_x = 0 if win_x < 0
            win_y = 0 if win_y < 0
            win_x = @width-1 if win_x >= @width
            win_y = @height-1 if win_y >= @height
            window << self[win_x, win_y]
          end
        end
        # median
        filtered[x, y] = window.sort[window.length / 2]
      end
      pb.update(y) if $DEBUG
    end

    pb.close if $DEBUG

    filtered
  end

  ###############################################
  def magnify(factor)
    bigger = self.class.new(@width * factor, @height * factor)
    each_pixel do |x,y|
      colour = self[x,y]
      (x*factor .. x*factor + factor-1).each do |xx|
        (y*factor .. y*factor + factor-1).each do |yy|
          bigger[xx,yy] = colour
        end
      end
    end
    bigger
  end

  ###############################################
  def histogram
    histogram = Hash.new(0)
    each_pixel do |x,y|
      histogram[self[x,y].luminosity] += 1
    end
    histogram 
  end

  ###############################################
  def draw_bezier_curve(points, colour)
    # ensure the points are increasing along the x-axis
    points = points.sort_by {|p| [p.x, p.y]}
    xmin = points[0].x
    xmax = points[-1].x
    increment = 2
    prev = points[0]
    ((xmin + increment) .. xmax).step(increment) do |x|
      t = 1.0 * (x - xmin) / (xmax - xmin)
      p = Pixel[x, bezier(t, points).round]
      draw_line(prev, p, colour)
      prev = p
    end
  end

  # the generalized n-degree Bezier summation
  def bezier(t, points)
    n = points.length - 1
    points.each_with_index.inject(0.0) do |sum, (point, i)|
      sum += n.choose(i) * (1-t)**(n - i) * t**i * point.y
    end
  end
 
  ###############################################
  def self.mandelbrot(width, height)
    mandel = Pixmap.new(width,height)
    pb = ProgressBar.new(width) if $DEBUG
    width.times do |x|
      height.times do |y|
        x_ish = Float(x - width*11/15) / (width/3)
        y_ish = Float(y - height/2) / (height*3/10)
        mandel[x,y] = RGBColour.mandel_colour(mandel_iters(x_ish, y_ish))
      end
      pb.update(x) if $DEBUG
    end
    pb.close if $DEBUG
    mandel
  end

  def self.mandel_iters(cx,cy)
    x = y = 0.0
    count = 0
    while Math.hypot(x,y) < 2 and count < 255
      x, y = (x**2 - y**2 + cx), (2*x*y + cy)
      count += 1
    end
    count
  end 

  ###############################################
  # Apply a convolution kernel to a whole image
  def convolute(kernel)
    newimg = Pixmap.new(@width, @height)
    pb = ProgressBar.new(@width) if $DEBUG
    @width.times do |x|
      @height.times do |y|
        apply_kernel(x, y, kernel, newimg)
      end
      pb.update(x) if $DEBUG
    end
    pb.close if $DEBUG
    newimg
  end

  # Applies a convolution kernel to produce a single pixel in the destination
  def apply_kernel(x, y, kernel, newimg)
    x0 = [0, x-1].max
    y0 = [0, y-1].max
    x1 = x
    y1 = y
    x2 = [@width-1, x+1].min
    y2 = [@height-1, y+1].min
 
    r = g = b = 0.0
    [x0, x1, x2].zip(kernel).each do |xx, kcol|
      [y0, y1, y2].zip(kcol).each do |yy, k|
        r += k * self[xx,yy].r
        g += k * self[xx,yy].g
        b += k * self[xx,yy].b
	    end
    end
    newimg[x,y] = RGBColour.new(luma(r), luma(g), luma(b))
  end

  # Function for clamping values to those that we can use with colors
  def luma(value)
    if value < 0
      0
    elsif value > 255
      255
    else
      value
    end
  end
end


###########################################################################
# Utilities
class ProgressBar
  def initialize(max)
    $stdout.sync = true
    @progress_max = max
    @progress_pos = 0
    @progress_view = 68
    $stdout.print "[#{'-'*@progress_view}]\r["
  end

  def update(n)
    new_pos = n * @progress_view/@progress_max
    if new_pos > @progress_pos
      @progress_pos = new_pos 
      $stdout.print '='
    end
  end

  def close
    $stdout.puts '=]'
  end
end

class Queue < Array
  alias_method :enqueue, :push
  alias_method :dequeue, :shift
end

class Numeric
  def fpart
    self - self.truncate
  end
  def rfpart
    1.0 - self.fpart
  end
end

class Integer
  def choose(k)
    self.factorial / (k.factorial * (self - k).factorial)
  end
  def factorial
    (2 .. self).reduce(1, :*)
  end
end
```


==A Test Suite==

```ruby
def display_pixmap(filename)
  puts "displaying #{filename}"
  system "./ppmview.rb #{filename} &"
end

###########################################################################
if $0 == __FILE__ 

  old_debug = $DEBUG
  $DEBUG = true

  # for testing
  class Pixmap
    def ==(a_bitmap)
      return false if @width != a_bitmap.width or @height != a_bitmap.height
      @width.times {|x| @height.times {|y| 
        return false if not self[x,y] == (a_bitmap[x,y])
      }}
      true
    end
  end

  require 'test/unit'
  class TestRGBColour < Test::Unit::TestCase
    def test_init
      color = RGBColour.new(0,100,200)
      assert_equal(100, color.g)
    end
    def test_constants
      assert_equal([255,0,0], [RGBColour::RED.r,RGBColour::RED.g,RGBColour::RED.b])
      assert_equal([0,255,0], [RGBColour::GREEN.r,RGBColour::GREEN.g,RGBColour::GREEN.b])
      assert_equal([0,0,255], [RGBColour::BLUE.r,RGBColour::BLUE.g,RGBColour::BLUE.b])
    end
    def test_error
      color = RGBColour.new(0,100,200)
      assert_raise(ArgumentError) {RGBColour.new(0,0,256)}
    end
  end
  class TestPixmap < Test::Unit::TestCase
    def setup
      @w = 20
      @h = 30
      @bitmap = Pixmap.new(@w,@h)
    end
    def test_init
      assert_equal(@w, @bitmap.width)
      assert_equal(@h, @bitmap.height)
      assert_equal(RGBColour::WHITE, @bitmap.get_pixel(10,10))
    end
    def test_fill
      @bitmap.fill(RGBColour::RED)
      assert_equal(255,@bitmap[10,10].red)
      assert_equal(0,@bitmap[10,10].green)
      assert_equal(0,@bitmap[10,10].blue)
    end
    def test_get_pixel
      assert_equal(@bitmap[5,6], @bitmap.get_pixel(5,6))
      assert_raise(ArgumentError) {@bitmap[100,100]}
    end
    def test_grayscale
      @bitmap.fill(RGBColour::BLUE)
      @bitmap.height.times {|y| [9,10,11].each {|x| @bitmap[x,y]=RGBColour::GREEN}}
      @bitmap.width.times  {|x| [14,15,16].each {|y| @bitmap[x,y]=RGBColour::GREEN}}
      @bitmap.save('testcross.ppm')
      Pixmap.open('testcross.ppm').to_grayscale.save('testgray.ppm')
    end
    def test_save
      @bitmap.fill(RGBColour::BLUE)
      filename = 'test.ppm'
      @bitmap.save(filename)
      expected_size = 3 + (@w.to_s.length + 1 + @h.to_s.length + 1) + 4 + (@w * @h * 3)
      assert_equal(expected_size, File.size(filename))
    end 
    def test_open
      @bitmap.fill(RGBColour::RED)
      @bitmap.set_pixel(10,15, RGBColour::WHITE)
      filename = 'test.ppm'
      @bitmap.save(filename)
      new = Pixmap.open(filename)
      assert(@bitmap == new)
    end
  end

  # a green cross on a blue background
  colour_bitmap = Pixmap.new(20, 30)
  colour_bitmap.fill(RGBColour::BLUE)
  colour_bitmap.height.times {|y| [9,10,11].each {|x| colour_bitmap[x,y]=RGBColour::GREEN}}
  colour_bitmap.width.times  {|x| [14,15,16].each {|y| colour_bitmap[x,y]=RGBColour::GREEN}}
  colour_bitmap.save('testcross.ppm')
  display_pixmap 'testcross.ppm'

  Pixmap.open('testcross.ppm').to_grayscale.save('testgray.ppm')

  image = Pixmap.open('testcross.ppm')
  image.save_as_jpeg('testcross.jpg')
  #image.print(:format => "P3")

  bitmap = Pixmap.open_from_jpeg('testcross.jpg')
  savefile = 'testcross_from_jpeg.ppm'
  bitmap.save(savefile)
  display_pixmap savefile

  bitmap = Pixmap.new(500, 500)
  bitmap.fill(RGBColour::BLUE)
  10.step(430, 60) do |a|
    bitmap.draw_line(Pixel[10, 10], Pixel[490,a], RGBColour::YELLOW)
    bitmap.draw_line(Pixel[10, 10], Pixel[a,490], RGBColour::YELLOW)
  end
  bitmap.draw_line(Pixel[10, 10], Pixel[490,490], RGBColour::YELLOW)
  savefile = 'testlines4.ppm'
  bitmap.save(savefile)
  display_pixmap savefile

  bitmap = Pixmap.new(30, 30)
  bitmap.draw_circle(Pixel[14,14], 12, RGBColour::BLACK)
  savefile = 'testcircle.ppm'
  bitmap.save(savefile)
  display_pixmap savefile

  bitmap = Pixmap.new(300, 300)
  bitmap.draw_circle(Pixel[149,149], 120, RGBColour::BLACK)
  bitmap.draw_circle(Pixel[200,100], 40, RGBColour::BLACK)
  bitmap.flood_fill(Pixel[140,160], RGBColour::BLUE)
  savefile = 'testflood.ppm'
  bitmap.save(savefile)
  display_pixmap savefile

  bitmap = Pixmap.new(500, 500)
  bitmap.fill(RGBColour::BLUE)
  10.step(430, 60) do |a|
    bitmap.draw_line_antialised(Pixel[10, 10], Pixel[490,a], RGBColour::YELLOW)
    bitmap.draw_line_antialised(Pixel[10, 10], Pixel[a,490], RGBColour::YELLOW)
  end
  bitmap.draw_line_antialised(Pixel[10, 10], Pixel[490,490], RGBColour::YELLOW)
  bitmap.save('testantialias.ppm')
  display_pixmap 'testantialias.ppm'

  file = 'teapot.ppm'
  display_pixmap file
  bitmap = Pixmap.open(file)
  # test new grayscale
  savefile = 'teapotgray.ppm'
  gray = bitmap.to_grayscale
  gray.save(savefile)
  display_pixmap savefile
  #
  savefile = 'testfiltered.ppm'
  filtered = bitmap.median_filter
  filtered.save(savefile)
  display_pixmap savefile

  file = 'teapot.ppm'
  savefile = 'teapotbw.ppm'
  display_pixmap file
  Pixmap.open(file).save_as_blackandwhite(savefile)
  display_pixmap savefile

  bitmap = Pixmap.new(400, 400)
  points = [
    Pixel[40,100], Pixel[100,350], Pixel[150,50], 
    Pixel[150,150], Pixel[350,250], Pixel[250,250]
  ]
  points.each {|p| bitmap.draw_circle(p, 3, RGBColour::RED)}
  bitmap.draw_bezier_curve(points, RGBColour::BLUE)
  savefile = 'testbezier.ppm'
  bitmap.save(savefile)
  display_pixmap savefile

  savefile = 'testmandel.ppm'
  Pixmap.mandelbrot(500,500).save(savefile)
  display_pixmap savefile
  
  # Demonstration code using the teapot image from Tk's widget demo
  teapot = Pixmap.open('teapot.ppm')
  [ ['Emboss',  [[-2.0, -1.0, 0.0],  [-1.0, 1.0, 1.0],  [0.0, 1.0, 2.0]]], 
    ['Sharpen', [[-1.0, -1.0, -1.0], [-1.0, 9.0, -1.0], [-1.0, -1.0, -1.0]]], 
    ['Blur',    [[0.1111,0.1111,0.1111],[0.1111,0.1111,0.1111],[0.1111,0.1111,0.1111]]],
  ].each do |label, kernel|
    savefile = 'test' + label.downcase + '.ppm'
    teapot.convolute(kernel).save(savefile)
    display_pixmap savefile
  end

  $DEBUG = old_debug 
end

```


==An Image Viewer==
{{libheader|Ruby/Tk}}

The <code>ppmview.rb</code> program is:

```ruby
#!/usr/bin/ruby

require 'tk'

if ARGV.empty?
  $stderr.puts "usage: #{File.basename($0)} imagefile"
  exit 1
end

filename = ARGV.shift
unless File.readable?(filename)
  raise ArgumentError, "can't read file '#{filename}'"
end

root = TkRoot.new('title' => File.basename(filename))
label = TkLabel.new(root) {image TkPhotoImage.new('file' => filename)}
label.pack
Tk.mainloop

```

