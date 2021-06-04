require_relative 'glyph.rb'

# Class to read .fnt files from Entergram's engine
class FntReader
  attr_reader :val1, :size, :val2, :header

  def initialize(file)
    @file = file
    read_file_header
  end

  # Reads a glyph by its numeric Unicode codepoint
  def read_glyph_by_codepoint(codepoint)
    read_glyph_by_address(@header[codepoint])
  end

  # Reads a glyph by numeric address within the file
  def read_glyph_by_address(address)
    @file.seek(address)
    offset_x, offset_y = @file.read(2).unpack('cc')
    crop_width, crop_height = @file.read(2).unpack('CC')
    frame_width, val6 = @file.read(2).unpack('CC')
    raise "val6 of glyph at #{address} is not 0 but #{val6}" if val6 != 0

    data_width, data_height = @file.read(2).unpack('CC')
    bytes_size, _ = @file.read(2).unpack('S<')
    glyph_data = @file.read(bytes_size)

    [
      offset_x, offset_y,
      crop_width, crop_height,
      frame_width, val6,
      data_width, data_height,
      bytes_size,
      glyph_data
    ]
  end

  private

  def read_file_header
    magic = @file.read(4)
    raise "Not an FNT4 file, magic bytes were: #{magic}" if magic != 'FNT4'

    @val1, @size, @val2 = @file.read(0xc).unpack('L<L<L<')
    @header = @file.read(0x40000).unpack('L*')
  end
end
