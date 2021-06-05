require_relative 'glyph.rb'
require_relative 'fnt_format.rb'
require_relative '../higu_compression.rb'

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
# 1. Reference .fnt file which will be used as a fallback
# 2. Folder from which new glyphs will be taken
# 3. Output path for .fnt file
reference_file_path, new_glyph_folder_path, out_path = ARGV

reference_file = FntReader.new(open(reference_file_path, 'rb'))
out_file = FntWriter.new(reference_file.val1, reference_file.val2)

# Add glyphs from reference file first
[*0x0000..0xffff].each do |codepoint|
  offset_x, offset_y, crop_width, crop_height, frame_width, val6, data_width, data_height, _, glyph_data = reference_file.read_glyph_by_codepoint(codepoint)
  out_file.set_glyph(codepoint, offset_x, offset_y, crop_width, crop_height,
    frame_width, val6, data_width, data_height, glyph_data)
end

# Converts a loaded PNG image to an array of 8-bit alpha values.
def png_to_8bit(png_image)
  bytes = png_image.pixels.map do |colour|
    unless ChunkyPNG::Color.grayscale?(colour)
      raise "Input PNG file at #{png_path} has non-grayscale pixels"
    end

    # Invert; black areas from the input file should be 0xff in the output
    255 - ChunkyPNG::Color.r(colour)
  end
end

FORMATS = [["1x", 128], ["0.5x", 64], ["0.25x", 32], ["0.125x", 16]]

# Load new glyphs from folder
glyph_folders = Dir[File.join(new_glyph_folder_path, "glyph_*")]
glyph_folders.each do |glyph_folder_path|
  # Parse folder name (called like "glyph_8243_316")
  _, codepoint, advance_width = File.basename(glyph_folder_path).split('_')
  codepoint = codepoint.to_i
  advance_width = advance_width.to_i

  data_width = nil

  png_images = FORMATS.map do |format, format_height|
    path = File.join(glyph_folder_path, format + ".png")
    image = ChunkyPNG::Image.from_file(path)

    # Set the data width to the next number divisible by 16 greater than the
    # width of the first image
    if data_width.nil?
      data_width = (image.width + 16) & 0xfff0
    end

    raise "Image height too small (#{image.height} < #{format_height})" if image.height < format_height
    target_width = data_width / (128 / format_height)

    # Some glyph images exported by FontForge end up bigger than the specified
    # size, because they have diacritics etc. that cause their height to go
    # above the glyph canvas height. We need to crop this from the bottom, so
    # the glyph does not end up vertically misaligned. This will cause the
    # diacritic to be partially cut off.
    # TODO: find a cleaner solution for this
    image.crop!(
      0, image.height - format_height, # x, y
      [image.width, target_width].min, format_height # width, height
    )

    # Resize image (filling with white to the right) so that it matches the data
    # width shifted down according to the format size
    new_image = ChunkyPNG::Canvas.new(target_width, image.height,
      ChunkyPNG::Color::WHITE)
    new_image.compose!(image)

    new_image
  end

  glyph_data = png_images.map { |image| png_to_8bit(image) }.flatten
  compressed = HiguCompression.compress_naive(glyph_data, 6).pack('C*')

  width, height = png_images[0].width, png_images[0].height
  raise "Height too big (#{height} > 128)" if height > 128

  out_file.set_glyph(
    codepoint,
    -8, 88, # x and y offsets
    width, height, # crop width and height
    advance_width * 128 / 1000, # frame width - converted from em units
    0, # val6
    data_width, 128, # data width and height
    compressed
  )

  puts "Replaced glyph #{[codepoint].pack('U')} (#{codepoint})"
end

# Export
out_file.write_to(open(out_path, 'wb'))
