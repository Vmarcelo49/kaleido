require 'fileutils'
require 'csv'
require 'stringio'
require 'json'

begin
  begin
    require 'oily_png' # try more performant native library first
    puts "successfully loaded oily_png"
  rescue LoadError
    require 'chunky_png'
    puts "oily_png is not available, png encoding will be very slow"
  end
  png_available = true
rescue LoadError
  puts "chunky_png or oily_png is not available, no png files will be written"
  png_available = false
end

# Arguments:
path = ARGV[0] # Input .fnt file
output_path = ARGV[1] # Output folder

file = open(path, 'rb')

magic = file.read(4)
if magic != 'FNT4'
  puts "Not an FNT4 file!".c(91)
  exit
end

val1, size, val2 = file.read(0xc).unpack('L<L<L<')
header = file.read(0x40000).unpack('L*')

FileUtils.mkdir_p output_path

def decompress_glyph(glyph_data, size)
  bytes = glyph_data.bytes
  marker = 1
  p = 0
  out_bytes = Array.new(size)

  while p < bytes.length
    # If we have exhausted the previous marker's bits, read the next marker.
    if marker == 1
      marker = 0x100 | bytes[p]
      p += 1
      next
    end

    if marker & 1 == 0
      # Read one byte
      b1 = bytes[p]
      p += 1

      out_bytes << b1
    else # marker & 1 == 1
      # Read two bytes
      b12 = bytes[p..p+1]
      p += 2

      b1, b2 = b12
      count = ((b1 & 0xfc) >> 2) + 3
      offset = ((b1 & 0x03) << 8) | b2

      count.times do
        r = out_bytes[-(offset + 1)]
        raise "Invalid lookback offset -#{offset}" if r.nil?
        out_bytes << r
      end
    end

    marker >>= 1
  end

  out_bytes.compact
end

def decompressed_to_png(decompressed, width, height)
  str = decompressed.map { |alpha| [0xff, 0xff, 0xff, alpha] }.flatten.pack('C*')
  ChunkyPNG::Image.from_rgba_stream(width, height, str)
end

manifest = {} # Glyphs by their index
lookup = {} # Glyphs by their address
Glyph = Struct.new(:index_hex, :address, :path, :offset_x, :offset_y, :crop_width, :crop_height, :frame_width, :val6, :data_width, :data_height, :decompressed)

puts "Found #{header.length} glyphs, #{header.uniq.length} unique"

header.each_with_index do |e, i|
  start_time = Time.now
  glyph_index_hex = i.to_s(16).rjust(4, '0')
  filename = glyph_index_hex + "_" + e.to_s(16)
  glyph_path = File.join(filename[0..1], filename)

  if lookup.key?(e)
    glyph = lookup[e]
  else
    file.seek(e)
    offset_x, offset_y, crop_width, crop_height, frame_width, val6, data_width, data_height = file.read(8).unpack('C*')
    raise "val6 of glyph #{glyph_index_hex} is not 0 but #{val6}" if val6 != 0

    bytes_size, _ = file.read(2).unpack('S<')
    glyph_data = file.read(bytes_size)
    decompressed = decompress_glyph(glyph_data, data_width * data_height * 2)

    glyph = Glyph.new(glyph_index_hex, e, glyph_path, offset_x, offset_y, crop_width, crop_height, frame_width, val6, data_width, data_height, decompressed)
    lookup[e] = glyph
  end

  manifest[i] = glyph

  raw_path = File.join(output_path, "raw", glyph_path + ".dat")
  FileUtils.mkdir_p File.dirname(raw_path)
  raw_file = open(raw_path, 'wb')
  raw_file.write(glyph_data)
  raw_file.close

  png_time = Time.now
  if png_available
    image = decompressed_to_png(glyph.decompressed, glyph.data_width, glyph.data_height)

    png_path = File.join(output_path, "1x", glyph_path + ".png")
    FileUtils.mkdir_p File.dirname(png_path)
    png_file = open(png_path, 'wb')
    image.write(png_file)
    png_file.close
  end

  # TODO: smaller sizes of glyphs

  puts "Wrote glyph file #{filename} in #{Time.now - start_time} seconds (of which png encoding: #{Time.now - png_time} seconds)"
end

File.write(File.join(output_path, "data.rb_marshal"), Marshal.dump(manifest))

json_friendly_manifest = Hash[manifest.map { |k, v| [k, v.to_h.except(:decompressed)] }]
File.write(File.join(output_path, "manifest.json"), JSON.pretty_generate(json_friendly_manifest))
