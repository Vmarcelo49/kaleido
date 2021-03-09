# Classes and utils for creating rom files

require 'stringio'
require 'digest'

PackedFile = Struct.new(:name, :folder?, :content)
FolderStruct = Struct.new(:data, :size, :flat_location)

class KalRom2File
  FILE_ALIGNMENT = 9 # Files are aligned to 2**9 bytes in rom files
  FOLDER_ALIGNMENT = 4 # Folders are aligned to 2**4 bytes

  attr_accessor :files

  def initialize(header_size = 0x24600, val1 = 1, val2 = 1)
    @header_size = header_size
    @next_file = header_size # where the next file should be written (at the end of the header, initially)

    @s = StringIO.new
    @s.binmode
    @s.write("ROM2")
    @s.write([val1, val2].pack('S<S<'))
    @header_pos = @s.pos

    @files = []
    @folder_data = {}
  end

  def write(path)
    header = StringIO.new
    header.binmode

    folders_to_process = [[:root, @files, nil, true]]
    folder_offsets = []

    until folders_to_process.empty?
      new_folders_to_process = []

      folders_to_process.each do |name, folder, parent_name, is_root|
        location = header.pos
        data, size, other_folder_offsets, subfolders = write_folder(name, folder, parent_name, is_root)

        # Add location of current folder in the header to all offsets, as the
        # references to other folders are relative to the header
        folder_offsets += other_folder_offsets.map { |o, name| [o + location, name] }
        new_folders_to_process += subfolders

        header.write(data)
        puts "Wrote folder #{name} to header"
        align = (1 << FOLDER_ALIGNMENT) - 1
        header.seek((header.pos + align) & ~align) # align next folder to 0x10
        @folder_data[name] = FolderStruct.new(data, size, location >> FOLDER_ALIGNMENT)
      end

      folders_to_process = new_folders_to_process
    end

    folder_offsets.each do |o, name|
      f = @folder_data[name]
      header.seek(o)
      header.write([f.flat_location, f.size].pack('L<L<'))
    end

    header.seek(0, IO::SEEK_END)
    align = (1 << FOLDER_ALIGNMENT) - 1
    fill = ((header.length + align) & ~align) - header.length
    header.write("\x00" * fill)

    raise "Header size too big" if header.length > @header_size

    @s.seek(@header_pos)
    puts "Length of header: 0x#{header.length.to_s(16)}"
    @s.write([header.length].pack('L<'))

    # Thanks to TellowKrinkle for letting me know what these values in the header are!
    @s.write [1 << FILE_ALIGNMENT].pack('L<')
    digest = Digest::MD5.digest(header.string)
    @s.write digest
    puts "MD5 of header: #{Digest.hexencode(digest)}"

    # Write the header itself
    @s.write(header.string)

    puts "Writing to string..."
    full_data = @s.string

    align = (1 << FILE_ALIGNMENT) - 1
    fill = ((full_data.length + align) & ~align) - full_data.length
    puts "Got data, writing to file..."
    f = File.open(path, 'wb')
    f.write(full_data)
    f.write("\x00" * fill)
    puts "Done"
  end

  private

  def write_folder(name, folder, parent_name, is_root = false)
    puts "write_folder #{name}"
    parent_name = name if is_root

    result = StringIO.new
    result.binmode
    name_offsets = []

    other_folder_offsets = []
    folders_to_process = []

    folder = [
      PackedFile.new('.', true, nil),
      PackedFile.new('..', true, nil)
    ] + folder

    result.write([folder.length].pack('L<'))

    folder.each do |file|
      name_offsets << result.pos # Mark file name position to be written here later
      result.write("\x00" * 3)
      if file.folder?
        result.write("\x80")
        case file.name
        when '.'
          other_folder_offsets << [result.pos, name]
        when '..'
          other_folder_offsets << [result.pos, parent_name]
        else
          puts "add folder #{file.name}"
          folders_to_process << [file.name, file.content, name, false]
          other_folder_offsets << [result.pos, file.name]
        end
        result.write("\x00" * 8)
      else
        result.write("\x00")
        length = file.content.bytes.length
        file_offset = @next_file
        @s.seek(file_offset)
        @s.write(file.content)

        align = (1 << FILE_ALIGNMENT) - 1
        @s.seek((@s.pos + align) & ~align) # align next file
        @next_file = @s.pos

        flat_offset = file_offset >> FILE_ALIGNMENT
        result.write([flat_offset, length].pack('L<L<'))
      end
    end

    # Write name section
    folder.each_with_index do |file, i|
      name_location = result.pos
      result.write(file.name + "\x00")
      cur = result.pos
      result.seek(name_offsets[i])
      name_location_bytes = [name_location].pack('L<')
      raise "Too many bytes" if name_location_bytes[3] != "\x00"
      result.write(name_location_bytes[0..2])
      result.seek(cur)
    end

    our_size = result.length

    [result.string, our_size, other_folder_offsets, folders_to_process]
  end
end

# Recursive method to turn a folder structure into nested PackedFiles, to be
# used for KalRom2File
def load_packed_file_from_folder_recursive(path, name = :root, replacements = {})
  result = []

  Dir.entries(path).sort.each do |entry|
    next if ['.', '..'].include? entry

    entry_path = File.join(path, entry)
    if File.directory?(entry_path)
      result << load_packed_file_from_folder_recursive(entry_path, entry, replacements)
    else
      if replacements.key?(entry)
        puts "Performing replacement: #{entry}"
        result << PackedFile.new(entry, false, replacements[entry])
      else
        result << PackedFile.new(entry, false, File.read(entry_path))
      end
    end
  end

  PackedFile.new(name, true, result)
end
