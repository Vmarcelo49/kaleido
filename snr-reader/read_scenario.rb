require 'set'
require 'digest'
require 'stringio'

# colourise string
class String; def c(a); "\e[#{a}m#{self}\e[0m"; end; end

# Arguments:
path = ARGV[0] # Input .snr file.
output_path = ARGV[1] # Output file. (Optional)
max_dialogue = ARGV[2] ? ARGV[2].to_i : 100000000 # Maximum amount of dialogue that will be parsed, useful if you are trying to quickly prototype some dialogue-independent functionality
dialogue_path = ARGV[3] # Optional; if present, a file containing only the raw dialogue lines will be written to this location.

# Calculate the SHA256 value of the given script, to find out which mode to use.
sha256 = Digest::SHA256.hexdigest(File.read(path))
puts "SHA256: #{sha256}"
if sha256 == '1a41c95be7427ddd3397249fde5be56dfd6f4a8cef20ab27a7a648f31e824dfb'
  load './assoc/kaleido.rb'
elsif sha256 == '1537bb6f964e2b3ce5501fc68d86f13b7b483d385f34ea6630a7e4d33758aa82'
  load './assoc/saku.rb'
elsif sha256 == 'f3be6c855e97d0442c9ec610d38e219d3696cf7e5da9c0f1b430d9df6d3f7130'
  load './assoc/konosuba.rb'
else
  ADDRESSES = {}
  REGISTERS = {}
  FF_CALLS = {}
  REQUIRE_LABELS = Set.new
  puts "Script not recognised! You are probably trying to load a different SNR file than Kal or Saku. This may or may not work."
end

file = open(path, 'rb')

# Byte lengths represented by Ruby pack/unpack instructions; these are the only supported ones in unpack_read
LENS = {
  'c' => 1,
  's' => 2,
  'l' => 4,
  'q' => 8
}

# Converter to be used for converting from SJIS to UTF-8
CONVERTER = Encoding::Converter.new('SHIFT_JIS', 'UTF-8', invalid: :replace)

# Entergram uses halfwidth katakana instead of hiragana, probably to save a bit of space. We have to reverse this.
# Each character in HALFWIDTH will be replaced with the corresponding one in HALFWIDTH_REPLACE
HALFWIDTH = '｢｣ｧｨｩｪｫｬｭｮｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝｰｯ､ﾟﾞ･?｡'
HALFWIDTH_REPLACE = '「」ぁぃぅぇぉゃゅょあいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをんーっ、？！…　。'

# File references, this is how assets must be stored relative to the output script path.
BG_FOLDER = 'bg'
BG_EXT = '.png'

SPRITE_FOLDER = 'sprites'

BGM_FOLDER = 'bgm'
SE_FOLDER = 'se'

# If true, certain internal instructions (NCSELECT) are ignored while parsing, as they
# are when playing the game normally. If false, it can be considered as
# "test mode"
# Probably only applies to Kal
SNR_PROD = true

SCREEN_WIDTH = 1920
SCREEN_HEIGHT = 1080

# Prints a set of bytes in the specified colour
def byte_print(array, c = 94)
  Kernel.puts array.flatten.map { |e| e.to_s(16).rjust(2, '0') }.join(' ').c(c)
end

# Stop if we are in the wrong mode (e.g. for specific instructions)
def assert_mode(mode)
  raise "Invalid mode (#{mode}) for current state, expected #{MODE}" if mode != MODE
end

# One of the biggest difference between Kal/Saku and previous games is that
# a new variable-length argument format is used for most instructions, rather
# than constant-length arguments.
# This format can not only represent constant values, but also references to
# registers and function parameters. Constant values are signed, stored in
# two's complement format.
class Varlen
  attr_reader :data, :mode, :value

  def initialize(data)
    @data = data
    @first_byte = data.bytes[0]

    if @first_byte >= 0x80 && @first_byte <= 0x8f
      # Three-byte number
      @mode = :m8
      @value = ((@first_byte & 0xF) << 8) | data.bytes[1]
      @value = @value - 0xfff if @value > 0x800 # Two's complement signed
    elsif @first_byte >= 0x90 && @first_byte <= 0x9f
      # Five-byte number
      @mode = :m9
      @value = ((@first_byte & 0xF) << 16) | (data.bytes[1] << 8) | data.bytes[2]
      @value = @value - 0xfffff if @value > 0x80000 # Two's complement signed
    elsif @first_byte >= 0xb0 && @first_byte <= 0xbf
      # Access a small register (%rx0 to %rxf)
      @mode = :mb
      @value = @first_byte & 0xf
    elsif @first_byte == 0xc0
      # Access a register larger than %rxf (i.e. %rx10 and onwards)
      @mode = :mc0
      @value = data.bytes[1]
    elsif @first_byte >= 0xd0 && @first_byte <= 0xdf
      # Access a function parameter
      @mode = :md
      @value = @first_byte & 0xf
    elsif @first_byte == 0xe0
      # Null value
      @mode = :me
      @value = 0
    elsif (@first_byte >= 0xa0 && @first_byte <= 0xaf) || (@first_byte >= 0xe1 && @first_byte <= 0xff) # these are, as far as I know, unused
      raise "Invalid varlen first byte: 0x#{@first_byte.to_s(16)}"
    elsif @first_byte >= 0x40 && @first_byte <= 0x7f
      # Seven-bit number (negative)
      @mode = :mraw
      @value = @first_byte - 128
    else
      # Seven-bit number (positive)
      @mode = :mraw
      @value = @first_byte
    end
  end

  # Does it represent a constant, non-null value?
  def constant?
    [:mraw, :m8, :m9].include? @mode
  end

  # Returns the constant value; raises an error if this varlen is not
  # constant. Honestly, this might even mean that the parameter is not even
  # supposed to be a varlen...
  def value!
    raise "Expected constant varlen but got #{self}" unless constant?
    @value
  end

  # Length in bytes of this varlen
  def length
    @data.length
  end

  def to_s
    hex = @data.bytes.map { |e| e.to_s(16).rjust(2, '0') }.join
    "V[#{@mode.to_s}, 0x#{hex}, #{@value}]"
  end
end

# Monkey-patch some methods into IO to make it easier to read certain things from the scenario file
class IO
  # Read some bytes and unpack into an array according to a given unpack
  # format specification.
  # Only supports C/c, S/s, L/l, Q/q at the moment (8, 16, 32, 64 bit, signed
  # or unsigned)
  def unpack_read(str)
    len = str.downcase.chars.map { |chr| LENS[chr] || 0 }.sum
    exit if len > 1000
    data = read(len)
    byte_print(data.bytes)
    result = data.unpack(str)
    p result
    result
  end

  # Read a SHIFT-JIS string of the given length. Must be null-terminated
  # (as all strings are in SNR format)
  def read_shift_jis(len)
    raw = read(len)
    raise 'Not null terminated!' unless raw.chars[-1] == 0.chr
    CONVERTER.convert(raw[0..-2]).tr(HALFWIDTH, HALFWIDTH_REPLACE)
  end

  # Read `len` varlen arguments to an instruction (see Varlen class above)
  def read_variable_length(len)
    result = []
    len.times do
      first_byte = read(1)
      if (first_byte.bytes[0] >= 0x80 && first_byte.bytes[0] <= 0x9f) || first_byte.bytes[0] == 0xc0
        # At least two bytes
        second_byte = read(1)
        first_byte += second_byte
        if first_byte.bytes[0] >= 0x90 && first_byte.bytes[0] <= 0x9f
          # Three bytes
          third_byte = read(1)
          first_byte += third_byte
        end
      end
      result << Varlen.new(first_byte)
    end
    # For debug purposes
    result.each { |v| Kernel.puts v.to_s.c(95) }
    result
  end

  # Read an asset table
  def read_table(offset, length_prefix = true)
    if offset == 0
      Kernel.puts "Warning: Offset for table is 0! Skipping" # somehow Konosuba does not have a BGM table???
      return
    end

    seek(offset)
    if length_prefix
      table_length, element_count = unpack_read('L<L<')
    else
      element_count, _ = unpack_read('L<')
    end
    element_count.times do |n|
      yield n
    end
  end
end

# Represents an nscripter script file that will be written.
# This class does most of the snr -> nsc transformation.
# If you intend to use a different output format, this is the class you will
# want to change.
class OutFile
  def initialize(address_offset, script_offset, debug = true)
    # Hash of instruction offset => nscripter lines
    @h = {}

    @debug = debug

    @offset = 0
    @address_offset = address_offset # useful when parsing script sections on their own, currently unused
    @script_offset = script_offset # determines when to write the interlude (= start of game section in nsc)
    @require_labels = REQUIRE_LABELS
    @nyi = false

    @known_functions = Set.new
    @known_registers = Set.new
    @known_parameters = Set.new

    @dialogue_lines = []

    # Counts which NScripter variable is to be used next to provide an alias for registers or function parameters.
    @nsc_variable_counter = 20 # reserve first 20 variables for internal use

    @masks = {}
    @backgrounds = {}
    @bustups = {}
    @bgm_tracks = {}
    @sound_effects = {}
    @movies = {}
    @voices = {}
    @table8 = {}
    @table9 = {}
  end

  attr_accessor :masks, :backgrounds, :bustups, :bgm_tracks, :sound_effects, :movies, :voices, :table8, :table9
  attr_reader :offset, :script_offset, :dialogue_lines

  def offset=(value)
    @nyi = false
    @offset = value
    @h[@offset] ||= []
  end

  def <<(line)
    if @nyi
      line = ";#{line} ;??"
    end
    @h[@offset] << line
  end

  def debug(line)
    return unless @debug
    @h[@offset] << ";#{line}"
  end

  def newline
    self << ""
  end

  # Write the created data to the given path
  def write(path)
    file = open(path, 'w')

    file.write(File.read('prelude.utf'))

    intermission_written = false

    # assign labels to locations used in jumps
    # combine lines to file, inserting labels
    # write to file
    @h.to_a.sort_by(&:first).each do |k, v|
      if !intermission_written && k >= @script_offset
        file.write(File.read('intermission.utf'))
        intermission_written = true
      end

      if @require_labels.include? k
        file.puts "*#{raw_address(k)}"
      end
      v.each { |line| file.puts line }
    end

    file.write(File.read('coda.utf'))
  end

  # ----------------------------------- #
  # - utility methods for consistency - #
  # ----------------------------------- #

  def register(num)
    return parameter(num & 0xf) if num >= 0x1000 && num <= 0x100f

    unless @known_registers.include? num
      @h[2] << "numalias #{raw_register(num).delete('%')}, #{@nsc_variable_counter}"
      @h[2] << "numalias #{REGISTERS[num].delete('%')}, #{@nsc_variable_counter}" if REGISTERS.key?(num)
      @nsc_variable_counter += 1
      @known_registers << num
    end
    REGISTERS[num] || raw_register(num)
  end

  def raw_register(num)
    "%rx#{num.to_s(16)}"
  end

  def parameter(num)
    unless @known_parameters.include? num
      @h[2] << "numalias #{raw_parameter(num).delete('%')}, #{@nsc_variable_counter}"
      @nsc_variable_counter += 1
      @known_parameters << num
    end
    raw_parameter(num)
  end

  def raw_parameter(num); "%px#{num.to_s(16)}"; end

  def address(num)
    num -= @address_offset
    @require_labels << num
    raw_address(num)
  end

  def addresses(nums)
    nums.map { |num| address(num) }
  end

  def raw_address(num)
    raw = "addr_0x#{num.to_s(16)}"
    ADDRESSES[raw] || raw
  end

  def hex(data)
    if data.is_a? Numeric
      "0x#{data.to_s(16)}"
    elsif data.is_a? String
      data.bytes.map { |e| e.to_s(16).rjust(2, '0') }.join(' ')
    elsif data.is_a? Array
      "[#{data.map { |e| hex(e) }.join(', ')}]"
    end
  end

  # If the given variable length object is constant, it will either return that
  # value itself or, if a block is given, the result of that block called with that value.
  # Otherwise it will return the closest NScripter equivalent to whatever the given varlen represents.
  def nscify(val)
    if val.constant?
      if block_given?
        yield(val.value)
      else
        val.value
      end
    else
      p val.value
      case val.mode
      when :mb
        # short register access
        register(val.value)
      when :mc0
        # long register access
        register(val.value)
      when :md
        # function parameter access
        parameter(val.value)
      when :me
        null
      end
    end
  end

  # Same as nscify, but if the value is equal to a given constant (specified in
  # the mappings by SPRITE_SLOT_MAIN) it will return the current_slot variable
  # instead.
  def nscify_slot(val)
    return "127 - %current_slot" if val.constant? && val.value == SPRITE_SLOT_MAIN
    "127 - #{nscify(val)}"
  end

  # Remove or change characters that would not be allowed in nsc identifiers
  def normalize(str)
    norm = str.tr("１２３４５６７８９０", "1234567890").gsub(/[^A-Za-z0-9_]/, '')
    norm = "X#{norm}" if norm =~ /^[0-9_]/
    norm
  end

  def background(val)
    nscify(val) { |value| raw_background(value) }
  end

  def raw_background(id); "bg_0x#{id.to_s(16)}_#{normalize(@backgrounds[id].name)}"; end

  def bustup(val) # probably unused
    nscify(val) { |value| raw_bustup(value) }
  end

  def raw_bustup(id); "bup_0x#{id.to_s(16)}_#{normalize(@bustups[id].name)}"; end

  def raw_bgm_track(id); "bgm_0x#{id}_#{normalize(@bgm_tracks[id].name1)}_#{normalize(@bgm_tracks[id].name2)}"; end

  def raw_sound_effect(id); "se_0x#{id.to_s(16)}_#{normalize(@sound_effects[id].name)}"; end

  def null; "null"; end

  def nyi
    @nyi = true
  end

  # ------------------------------------------------ #
  # - utility methods for frequently-used NSC code - #
  # ------------------------------------------------ #

  def read_sprite_cache_for_sp2(slot)
    "?sprite_x_positions[#{slot}] + #{SCREEN_WIDTH / 2}, ?sprite_y_positions[#{slot}] + #{SCREEN_HEIGHT / 2}, ?sprite_x_scales[#{slot}] / 10, ?sprite_y_scales[#{slot}] / 10, ?sprite_rotation_angles[#{slot}]"
  end

  def load_sprite_cached(slot, to_load)
    self << %(lsph #{slot}, #{to_load}, 0, 0) # regular hidden sprite, for measuring purposes
    self << %(lsp2 #{slot}, #{to_load}, #{read_sprite_cache_for_sp2(slot)})
  end

  def sprite_cache_set(slot, what, value)
    self << %(mov ?sprite_#{what}s[#{slot}], #{value})
    self << %(amsp2 #{slot}, #{read_sprite_cache_for_sp2(slot)})
  end

  # --------------------------------------- #
  # - methods for individual instructions - #
  # --------------------------------------- #

  def dialogue(num, var1, length, str)
    debug "DIALOGUE: line #{num}, var1 #{var1}"
    debug "raw: #{str}"
    components = str.split(/(?=@.)/)

    character_name = nil
    result_str = ""
    furi1, furi2 = nil, nil

    components.each do |e|
      if e.start_with? '@'
        tag, content = [e[0..1], e[2..-1]]
        case tag
        when "@k" # click wait
          result_str += "@#{content}"
        when "@r" # newline
          result_str += "\n^#{content}"
        when "@v" # play voice
          voice_name, text = content.split(".")
          result_str += %(/\nwave "voice/#{voice_name}.wav"\n^#{text})
        when "@b" # begin furigana
          furi1 = content[0..-2] # remove trailing period
        when "@<" # begin what the furigana refers to
          furi2 = content
        when "@>" # end furigana
          # TODO: find a way to actually display furigana (or other ruby text)
          # in Ponscripter. It does not have the ruby text support of other
          # NScripter-type engines, so it is not possible natively. An old
          # (supposedly outdated) manual file reads:
          #
          #   Both [ruby and tategaki] can be simulated in small
          #   quantities if required by judicious use of h_textextent and font
          #   size/position tags.
          #
          # Using h_textextent to measure the text for correct centering of the
          # ruby text makes sense, but I cannot figure out how to use the result
          # within position tags, or even how to interpolate variables into
          # text formatting tags in general.
          # For now, use parentheses instead.
          result_str += "#{furi2}(#{furi1})#{content}"
        when "@[" # begin comment?
          result_str += "[#{content}"
        when "@]" # end comment?
          result_str += "]#{content}"
        when "@|" # defines pipe to be waited for. NYI
          result_str += "#{content}"
        when "@y" # after the pipe wait is finished. NYI
          result_str += "#{content}"
        when "@w" # waiting for a specified amount of time?
          result_str += "/\n; wait #{content}\n^"
        when "@o" # NYI
        when "@a" # NYI
        when "@z" # NYI
        when "@s" # NYI
        when "@{" # NYI
        when "@}" # NYI
        when "@e" # NYI
        when "@c" # NYI
        when "@t" # NYI
        when "@-" # NYI
        else
          raise "Unrecognised dialogue tag: #{tag}"
        end
      else
        character_name = e
      end
    end

    result_str.strip!

    self << "^#{character_name} ~y+10~" unless character_name.nil?
    self << "#{result_str}@" # actual line
    @dialogue_lines << result_str
    self << "textclear"
    newline
  end

  def ins_0x87(argument)
    nyi
    debug "instruction 0x87 (dialogue pipe wait?), argument: #{hex(argument)}"
  end

  def ins_0x88
    nyi
    debug "instruction 0x88"
  end

  def ins_0x89(argument, val1)
    nyi
    debug "instruction 0x89 (hide dialogue window?), argument: #{hex(argument)}, val1: #{val1}"
  end

  def ins_0x8a(argument)
    nyi
    debug "instruction 0x8a, argument: #{hex(argument)}"
  end

  def ins_0x8b(data)
    nyi
    debug "instruction 0x8b, argument: #{hex(data)}"
  end

  def ins_0x8d(val1, val2, register, val3, code, data)
    debug "instruction 0x8d, val1: #{hex(val1)}, val2: #{hex(val2)}, register: #{hex(register)}, val3: #{val3}, code: '#{code}', data: '#{data}'"

    if code == "NCSELECT"
      ncselect(data.split("\x00").compact)
    else
      nyi
    end
  end

  def ins_0x8e(val1, val2, val3, data)
    nyi
    debug "instruction 0x8e (related to 0xc9), val1: #{val1}, val2: #{val2}, val3: #{val3}, data: #{data}"
  end

  def ins_0xc9(val1, val2, val3, data)
    nyi
    debug "instruction 0xc9 (related to 0x8e), val1: #{val1}, val2: #{val2}, val3: #{val3}, data: #{data}"
  end

  def ins_0x8f
    nyi
    debug "instruction 0x8f"
  end

  # Register stuff

  def register_signed_assign(reg, value)
    self << "mov #{register(reg)}, #{nscify(value)} ; #{value}"
  end

  def register_unsigned_assign(reg, value)
    self << "mov #{register(reg)}, #{nscify(value)} ;unsigned #{value}"
  end

  def register_add(reg, value)
    self << "mov #{register(reg)}, #{register(reg)} + #{nscify(value)} ; #{value}"
  end

  def register_sub(reg, value)
    self << "mov #{register(reg)}, #{register(reg)} - #{nscify(value)} ; #{value}"
  end

  def register_mul(reg, value)
    self << "mov #{register(reg)}, #{register(reg)} * #{nscify(value)} ; #{value}"
  end

  def register_div(reg, value)
    self << "mov #{register(reg)}, #{register(reg)} / #{nscify(value)} ; #{value}"
  end

  def register_or(reg, value)
    nyi
    debug "mov #{register(reg)}, #{register(reg)} | #{value}"
  end

  def register_and(reg, value)
    nyi
    debug "mov #{register(reg)}, #{register(reg)} & #{value}"
  end

  def register_0x08(reg, value) # something special to kaleido. (Couldn't find this in saku) implementing as + for the time being...
    nyi
    self << "mov #{register(reg)}, #{register(reg)} [0x08] #{nscify(value)} ; #{value}"
  end

  def register_add2(reg, value1, value2)
    self << "mov #{register(reg)}, #{nscify(value1)} + #{nscify(value2)} ; #{value1} #{value2}"
  end

  def register_sub2(reg, value1, value2)
    self << "mov #{register(reg)}, #{nscify(value1)} - #{nscify(value2)} ; #{value1} #{value2}"
  end

  def register_0x84(reg, value1, value2)
    nyi
    debug "mov #{register(reg)}, #{register(reg)} [0x84] #{value1} #{value2} ; potentially two argument multiplication?"
  end

  def register_0x85(reg, value1, value2)
    nyi
    debug "mov #{register(reg)}, #{register(reg)} [0x85] #{value1} #{value2} ; potentially two argument division?"
  end

  def register_0x86(reg, value1, value2)
    nyi
    debug "mov #{register(reg)}, #{register(reg)} [0x86] #{value1} #{value2} ; potentially two argument or?"
  end

  def register_0x87(reg, value1, value2)
    nyi
    debug "mov #{register(reg)}, #{value1} [0x87] #{value2} ; potentially two argument and?"
  end

  # Other register stuff

  def ins_0x40(val1, val2, val3)
    nyi
    debug "instruction 0x40, val1: #{val1}, val2: #{val2}, val3: #{val3}"
  end

  # The SNR file uses a stack-based operation language for complex calculations.
  # This class represents one of those operations being parsed and converted to
  # NSC.
  class CalcStack
    def initialize
      @stack = []
      @result = []
    end

    def push(val)
      @stack.push(val)
    end

    def pop
      @stack.pop
    end

    def raw_stack
      @stack
    end

    # A simple binary operation that can be expressed with a single operator in nsc.
    def simple_binary(operator)
      second = @stack.pop
      @stack.push(['(', @stack.pop, " #{operator} ", second, ')'])
    end

    # A simple unary operation that can be expressed with a single operator in nsc.
    def simple_unary(operator)
      @stack.push(["(#{operator} ", @stack.pop, ')'])
    end

    # Stores the calculation up to the current point in %i1, so a more complex
    # calculation can be performed in the meantime. The calculation will be
    # resumed with %i2.
    def intermediate
      @result << "mov %i1, #{nested_array_to_nsc(@stack.pop)} ; calc intermediate"
      push('%i1')
      yield
      push('%i2')
    end

    def finalize(register)
      @result << "mov #{register}, #{nested_array_to_nsc(@stack.pop)} ; calc final"
      @result.join("\n")
    end

    def <<(line)
      @result << line
    end

    def empty?
      @stack.empty?
    end

    def nested_array_to_nsc(nested)
      raise 'nested is nil' if nested.nil?
      [nested].flatten.join
    end
  end

  def calc(target, operations)
    stack = CalcStack.new

    operations.each do |op, val|
      case op
      when 0x00 # push
        stack.push(nscify(val))
      when 0x01
        stack.simple_binary('+')
      when 0x02
        stack.simple_binary('-')
      when 0x03
        stack.simple_binary('*')
      when 0x04
        stack.simple_binary('/')
      when 0x0b
        nyi
        stack.simple_unary('[0x0b]')
      when 0x10 # probably: 0 if to_check < to_compare, 1 if to_check >= to_compare
        stack.intermediate do
          to_compare = stack.pop
          to_check = stack.pop
          stack << "if #{to_check} < #{to_compare}: mov %i2, 0"
          stack << "if #{to_check} >= #{to_compare}: mov %i2, 1"
        end
      when 0x18 # ternary operator? to_check ? true_val : false_val
        stack.intermediate do
          to_check = stack.pop
          true_val = stack.pop
          false_val = stack.pop
          stack << "if #{to_check} > 0: mov %i2, #{true_val}"
          stack << "if #{to_check} <= 0: mov %i2, #{false_val}"
        end
      else
        nyi
        stack.simple_binary("[0x#{op.to_s(16)}]")
      end
    end

    self << stack.finalize(register(target))

    unless stack.empty?
      nyi
      debug "Could not fully parse expression! Stack at the end: #{stack.raw_stack}"
    end
    #self << "mov #{register(target)}, #{stack.flatten.join} ; calc #{operations.map { |e| e.length == 2 ? [hex(e.first), e.last.to_s] : [hex(e.first)] }.join(' ')}"
  end

  def store_in_multiple_registers(value, registers)
    self << registers.map.with_index { |e, i| "mov #{register(e)}, #{nscify(value)}" }.join(':') + " ; #{value}"
  end

  # Read values[index] to target
  def lookup_read(target, index, values)
    self << "movz ?lookup, #{values.map { |e| nscify(e) }.join(', ')} ; #{values.map(&:to_s)} ; lookup_read"
    self << "mov #{register(target)}, ?lookup[#{nscify(index)}] ; #{index}"
  end

  # Store the given value in the register determined by registers[index]
  def lookup_store(value, index, registers)
    self << "movz ?lookup, #{registers.map { |e| register(e) }.join(', ')} ; lookup_store"
    self << "mov ?lookup[#{nscify(index)}], #{nscify(value)} ; value: #{value}, index: #{index}"
    self << registers.map.with_index { |e, i| "mov #{register(e)}, ?lookup[#{i}]" }.join(':')
  end

  # Control flow

  def call(addr, data)
    self << "#{address(addr)} #{data.map { |e| nscify(e) }.join(', ')} ; #{data.map(&:to_s).join(', ')}"
    unless @known_functions.include?(addr)
      @h[1] << "defsub #{address(addr)}"
      lines_at_func = (@h[addr] ||= [])
      lines_before = []

      # Save parameter values that might be overwritten to the pstack
      data.each_with_index do |_, i|
        lines_before << "inc %psp:mov ?param_stack[%psp], #{parameter(i)}"
      end
      lines_before << "inc %psp:mov ?param_stack[%psp], #{data.length}"
      # lines_before << "^call0x#{addr.to_s(16)},^%psp^,^#{data.length}^-^?param_stack[%psp]^/"
      if data.length > 0
        # Load new parameter into variable
        lines_before << "getparam " + data.map.with_index { |_, i| parameter(i) }.join(", ")
      end
      lines_at_func.unshift(lines_before)
      @known_functions << addr
    end
  end

  def unconditional_jump(addr)
    p addr
    self << "goto *#{address(addr)}"
  end

  def conditional_jump(val1, val2, addr, comparison)
    self << "if #{nscify(val1)} #{comparison} #{nscify(val2)} goto *#{address(addr)} ; #{val1} #{val2}"
  end

  def conditional_jump_equal(val1, val2, addr)
    conditional_jump(val1, val2, addr, "==")
  end

  def conditional_jump_inequal(val1, val2, addr)
    conditional_jump(val1, val2, addr, "!=")
  end

  def conditional_jump_greater_or_equal(val1, val2, addr)
    conditional_jump(val1, val2, addr, ">=")
  end

  def conditional_jump_greater_than(val1, val2, addr)
    conditional_jump(val1, val2, addr, ">")
  end

  def conditional_jump_less_or_equal(val1, val2, addr)
    conditional_jump(val1, val2, addr, "<=")
  end

  def conditional_jump_less_than(val1, val2, addr)
    conditional_jump(val1, val2, addr, "<")
  end

  def conditional_jump_0x06(val1, val2, addr) # conjecture: checks whether bit is set
    # Ponscripter does not support logical operations, so we have to do this using division...
    self << "if (#{nscify(val1)} / #{nscify(val2)}) mod 2 == 1 goto *#{address(addr)} ; #{val1} #{val2}"
  end

  def conditional_jump_0x86(val1, val2, addr) # MAYBE checks whether a bit is not set? this could mean that [0x8X] = ![0x0X]
    # nyi; conditional_jump(val1, val2, addr, "[0x86]")
    self << "if (#{nscify(val1)} / #{nscify(val2)}) mod 2 == 0 goto *#{address(addr)} ; #{val1} #{val2}"
  end

  def ins_0x48(addr)
    nyi
    debug "instruction 0x48 (gosub?), addr: #{address(addr)}"
  end

  def ins_0x49
    nyi
    debug "instruction 0x49 (return?)"
  end

  def table_goto(value, targets)
    self << "tablegoto #{nscify(value)}, #{addresses(targets).join(', ')} ; #{value}"
  end

  def ins_0x4b(register, targets)
    nyi
    debug "instruction 0x4b, register: #{hex(register)}, targets: #{addresses(targets)}"
  end

  def ins_0x4c(data)
    nyi
    debug "instruction 0x4c, data: #{hex(data)}"
  end

  def return
    #self << "^return_at 0x#{@offset.to_s(16)}/"
    self << "restore_params:return ;0x#{@offset.to_s(16)}"
    newline
  end

  def ins_0x51(reg, val3, val4, data) # conjecture: matching something to a set of values?
    if val4.value != 0
      puts "invalid val4 #{hex(val4)}"
      exit
    end
    self << "mov %i1, null ; 0x51 val3: #{val3}"
    data.each_with_index do |e, i|
      self << "if #{nscify(val3)} == #{e}: mov %i1, #{i}"
    end
    self << "mov #{register(reg)}, %i1"
  end

  def ins_0x52
    nyi
    debug "instruction 0x52 (some kind of return?)"
  end

  def ins_0x53(reg, val1, val2)
  debug "instruction 0x53: register: #{hex(reg)}, val1: #{val1}, val2: #{val2}"
  end

  def end
    nyi
    debug "end"
  end

  def ins_0x80(reg, val1)
    nyi
    debug "instruction 0x80: register: #{hex(reg)}, val1: #{val1}"
  end

  def ins_0x81(register, val1)
    nyi
    debug "instruction 0x81: register: #{hex(register)}, val1: #{val1}"
  end

  def ins_0x82(data)
    nyi
    debug "instruction 0x82: #{hex(data)}"
  end

  def wait_frames(mode, num_frames)
    self << "mov %i1, #{nscify(num_frames)} * 16 ; wait #{num_frames} frames" # TOOD: more accurate timing
    self << "wait %i1"
  end

  # Most likely sets the current textbox mode. (ADV/NVL, or special positions maybe?)
  def ins_0x85(mode)
    # 0 = ADV (default)
    # 1 = NVL-ish
    debug "instruction 0x85 (set text positioning mode), mode: #{mode}"
    #self << "^ins 0x85, val: ^#{nscify(val1)}"
  end

  # Stack related?

  def stack_push(values)
    values.each do |value|
      self << "inc %sp:mov ?stack[%sp], #{nscify(value)} ; push #{value}"
    end
  end

  def stack_pop(values)
    values.each do |value|
      self << "mov #{register(value)}, ?stack[%sp]:dec %sp ; pop #{hex(value)}"
    end
  end

  def ins_0xff(code, arguments) # most likely an external/internal call
    debug "instruction 0xff (internal call) at 0x#{@offset.to_s(16)}, code: '#{code}', arguments: #{arguments.map(&:to_s).join(', ')}"
    if FF_CALLS.key? code
      self << "#{FF_CALLS[code]} #{arguments.map { |e| nscify(e) }.join(', ')}"
    else
      nyi
      self << %(#{code} #{arguments.map { |e| nscify(e) }.join(', ')})
    end
  end

  # An internal selection operation. Sets %r1 to the index of the selected item.
  # In production use, it will always return the last index (?)
  def ncselect(options)
    debug "NCSELECT #{options.join(', ')}"
    if SNR_PROD
      self << "mov #{register(1)}, #{options.length - 1}"
    else
      raise "Test mode NCSELECT is currently not implemented!"
    end
  end

  # Sprites, resources

  def resource_command_0x0(slot, val1, val2)
    # The values, what do they mean?

    debug "resource command (0xc1) 0x0 (remove slot?), slot #{slot}, values: #{val1} #{val2}"
    self << "vsp2 #{nscify_slot(slot)}, 0"
  end

  def resource_command_0x1(slot, val1, val2, val3, val4, val5, width, height)
    nyi
    debug "resource command (0xc1) 0x1 (load simple?), slot #{slot}, values: #{val1} #{val2} #{val3} #{val4} #{val5} width: #{width} height: #{height}"
  end

  def load_background_0x0(slot, val1)
    debug "resource command (0xc1) 0x2 (load background) mode 0x0, slot #{slot}, val1: #{val1}"
  end

  def load_background_0x1(slot, val1, picture_index)
    debug "resource command (0xc1) 0x2 (load background) mode 0x1, slot #{slot}, val1: #{val1}, picture index: #{picture_index}"

    self << "#{LookupTable.for("background")} #{nscify(picture_index)}"
    load_sprite_cached(nscify_slot(slot), '$i2')
  end

  def load_background_0x3(slot, val1, picture_index, val3)
    debug "resource command (0xc1) 0x2 (load background) mode 0x3, slot #{slot}, val1: #{val1}, picture index: #{picture_index}, val3: #{val3}"

    self << "#{LookupTable.for("background")} #{nscify(picture_index)}"
    load_sprite_cached(nscify_slot(slot), '$i2')
  end

  def load_sprite(slot, val1, mode, sprite_index, val4, face_id, val6)
    debug "resource command (0xc1) 0x3 (load bustup sprite), slot #{slot}, values: #{val1} #{mode} #{sprite_index} val4: #{val4}, face_id: #{face_id}, val6: #{val6}"

    case mode
    when 0x00 # sometimes used in saku
      self << "^load sprite slot=^#{nscify(slot)}^ val1=^#{nscify(val1)}^ mode=^#{mode}}"
    when 0x01 # saku default
      self << "^load sprite slot=^#{nscify(slot)}^ val1=^#{nscify(val1)}^ mode=^#{mode}^ sprite_index=^#{nscify(sprite_index)}}"
      self << "#{LookupTable.for("bustup")} #{nscify(sprite_index)}"
      load_sprite_cached(nscify_slot(slot), 'c_sprite_folder + "\" + $i2 + "_" + $i3 + "_1.png"')
    when 0x0f # kal default
      self << "#{LookupTable.for("bustup")} #{nscify(sprite_index)}"
      self << "itoa_pad $i3, #{nscify(face_id)}, 3"
      load_sprite_cached(nscify_slot(slot), 'c_sprite_folder + "\" + $i2 + "_" + $i3 + ".png"')
    else
      self << "^load sprite slot=^#{nscify(slot)}^ val1=^#{nscify(val1)}^ mode=^#{mode}^ sprite_index=^#{nscify(sprite_index)}}"
    end
  end

  def resource_command_0x4(slot, val1, val2)
    nyi
    debug "resource command (0xc1) 0x4 (anime_load?), slot #{slot}, values: #{val1} #{val2}"
  end

  def resource_command_0x6_0x1(slot, val1, data)
    nyi
    debug "resource command (0xc1) 0x6 0x2, slot #{slot}, values: #{val1}, data: #{data}"
  end

  def resource_command_0x6_0x2(slot, val1, data)
    nyi
    debug "resource command (0xc1) 0x6 0x2, slot #{slot}, values: #{val1}, data: #{data}"
  end

  def resource_command_0x6_0x3(slot, val1, val3, data)
    nyi
    debug "resource command (0xc1) 0x6 0x3, slot #{slot}, values: #{val1} #{val3}, data: #{data}"
  end

  def resource_command_0x6_0x5(slot, val1, val3, data)
    nyi
    debug "resource command (0xc1) 0x6 0x5, slot #{slot}, values: #{val1} #{val3}, data: #{data}"
  end

  def resource_command_0x8(slot, val1, val2, val3, val4)
    nyi
    debug "resource command (0xc1) 0x8, slot #{slot}, values: #{val1} #{val2} #{val3} val4: #{val4}"
  end

  def resource_command_0x9(slot, val1, val2, val3)
    nyi
    debug "resource command (0xc1) 0x9 (special?), slot #{slot}, values: #{val1} #{val2} #{val3}"
  end

  def sprite_command_hide(slot)
    self << "vsp2 #{nscify_slot(slot)}, 0"
  end

  def sprite_command_0x01(slot)
    nyi
    debug "sprite command (0xc2) 0x01 (alpha?)"
  end

  def sprite_command_0x12(slot)
    nyi
    debug "sprite command (0xc2) 0x12 (y resize?)"
  end

  def sprite_wait_0x00(slot, val2)
    nyi
    #@h[@offset] << "^spritewait 0x00 slot=^#{nscify(slot)}^,val2=^#{nscify(val2)}"
    debug "sprite wait (0xc3) 0x00, values: #{slot} #{val2}"
  end

  def sprite_set_basic_transform(slot, target, value) # used frequently in saku
    #@h[@offset] << "^spritewait 0x01 slot=^#{nscify(slot)}^,target=^#{nscify(target)}^,value=^#{nscify(value)}"
    debug "sprite wait (0xc3) 0x01 (alpha?), values: #{slot} #{target} #{value}"

    case target.value!
    when 0x00 # X position?
      self << "mov %i2, #{nscify(value)} + 0"
      sprite_cache_set(nscify_slot(slot), "x_position", "%i2")
    when 0x05 # Y position?
      self << "mov %i2, #{nscify(value)} + 0"
      sprite_cache_set(nscify_slot(slot), "y_position", "%i2")
    else
      nyi
    end
  end

  def sprite_wait_0x02(slot, val2, val3)
    nyi
    #@h[@offset] << "^spritewait 0x02 slot=^#{nscify(slot)}^,val2=^#{nscify(val2)}^,val3=^#{nscify(val3)}"
    debug "sprite wait (0xc3) 0x02, values: #{slot} #{val2} #{val3}"
  end

  def sprite_wait_0x03(slot, val2, val3, val4)
    nyi
    #@h[@offset] << "^spritewait 0x03 slot=^#{nscify(slot)}^,val2=^#{nscify(val2)}^,val3=^#{nscify(val3)}^,val4=^#{nscify(val4)}"
    debug "sprite wait (0xc3) 0x03, values: #{slot} #{val2} #{val3} #{val4}"
  end

  def sprite_wait_0x04(slot, val2, val3)
    nyi
    #@h[@offset] << "^spritewait 0x04 slot=^#{nscify(slot)}^,val2=^#{nscify(val2)}^,val3=^#{nscify(val3)}"
    debug "sprite wait (0xc3) 0x04, values: #{slot} #{val2} #{val3}"
  end

  def sprite_wait_0x05(slot, val2, val3, val4)
    nyi
    #@h[@offset] << "^spritewait 0x05 slot=^#{nscify(slot)}^,val2=^#{nscify(val2)}^,val3=^#{nscify(val3)}^,val4=^#{nscify(val4)}"
    debug "sprite wait (0xc3) 0x05 (x pos?), values: #{slot} #{val2} #{val3} #{val4}"
  end

  def sprite_wait_0x06(slot, val2, val3, val4)
    nyi
    #@h[@offset] << "^spritewait 0x06 slot=^#{nscify(slot)}^,val2=^#{nscify(val2)}^,val3=^#{nscify(val3)}^,val4=^#{nscify(val4)}"
    debug "sprite wait (0xc3) 0x06 (y pos?), values: #{slot} #{val2} #{val3} #{val4}"
  end

  def sprite_wait_0x07(slot, val2, val3, val4, val5)
    nyi
    #@h[@offset] << "^spritewait 0x07 slot=^#{nscify(slot)}^,val2=^#{nscify(val2)}^,val3=^#{nscify(val3)}^,val4=^#{nscify(val4)}^,val5=^#{nscify(val5)}"
    debug "sprite wait (0xc3) 0x07, values: #{slot} #{val2} #{val3} #{val4} #{val5}"
  end

  def sprite_set_complex_transform(slot, target, value, duration, val5, val6) # used often in kal
    debug "sprite wait (0xc3) 0x0f, values: #{slot} #{target} #{value} #{duration} #{val5} #{val6}"
    #self << "^spritewait 0x0f slot=^#{nscify(slot)}^,target=^#{nscify(target)}^,value=^#{nscify(value)}^,duration=^#{nscify(duration)}^,val5=^#{nscify(val5)}^,val6=^#{nscify(val6)}"
    case slot.value!
    when SPRITE_SLOT_MAIN
      case target.value!
      when 0x00 # X position
        #self << "getspsize #{nscify_slot(slot)}, %i1, %i2"
        #self << "mov %i2, #{nscify(duration)} + %i2 / 2" # It seems that the sprite X position is based on the center of the sprite, while the Y position uses the top.
        #self << "mov ?sprite_x_positions[#{nscify_slot(slot)}], #{nscify(value)}"
        #self << "mov ?sprite_y_positions[#{nscify_slot(slot)}], %i2"
        #self << "amsp2 #{nscify_slot(slot)}, #{nscify(value)} + 960, %i2, 100, 100, 0, 255"
        self << "mov %i2, #{nscify(value)} + 0"
        sprite_cache_set(nscify_slot(slot), "x_position", "%i2")
      when 0x01 # Y position
        self << "mov %i2, #{nscify(value)} + 0"
        sprite_cache_set(nscify_slot(slot), "y_position", "%i2")
      when 0x0c # X (?) scaling
        self << "mov %i2, #{nscify(value)} / 1"
        sprite_cache_set(nscify_slot(slot), "x_scale", "%i2")
      when 0x0d # Y (?) scaling
        self << "mov %i2, #{nscify(value)} / 1"
        sprite_cache_set(nscify_slot(slot), "y_scale", "%i2")
      else
        # 0x1 = background
        # 0xd, 0xc, 0x9, 0x12 = ?
        nyi
      end
    else
      nyi
    end
  end

  def ins_0xc0(slot)
    nyi
    debug "instruction 0xc0, slot: #{hex(slot)}"
  end

  def ins_0xc4(target, data)
    nyi
    debug "instruction 0xc0, target: #{target}, data: #{data}"
  end

  def ins_0xc5(id1)
    nyi
    debug "instruction 0xc5 (set current slot?), id: #{id1}"
  end

  # WILD GUESS: this sets the value of the 0x7a (-6) sprite slot? unclear
  # what the second ID is for though
  def ins_0xc6(id1, id2)
    debug "instruction 0xc6 (set current slot?), id1: #{id1}, id2: #{id2}"
    self << "mov %current_slot, #{nscify(id1)}"
  end

  def ins_0xc7(slot, command)
    nyi
    debug "instruction 0xc7 (some sprite command?), slot: #{slot}, command: #{hex(command)}"
  end

  def ins_0xca(register)
    nyi
    debug "instruction 0xca, slot/register: #{hex(register)}"
  end

  def ins_0xcb
    nyi
    debug "instruction 0xcb (waiting for something?)"
  end

  def ins_0xcc(val1)
    nyi
    debug "instruction 0xcc, val1: #{hex(val1)}"
  end

  def ins_0xcd
    nyi
    debug "instruction 0xcd"
  end

  def ins_0xce(val1, val2, val3)
    nyi
    debug "instruction 0xce, val1: #{val1}, val2: #{val2}, val3: #{val3}"
  end

  # Sound related

  def play_bgm(bgm_id, fadein_frames, loop_flag, volume)
    # loop_flag is conjectured; it is always 0 in Kal
    debug "Play BGM, bgm_id: #{bgm_id}, fadein_frames: #{fadein_frames}, loop_flag: #{loop_flag}, volume: #{volume}"
    self << %(bgmvol #{nscify(volume)})
    #self << "mp3fadein #{nscify(fadein_frames)}"
    self << "#{LookupTable.for("bgm")} #{nscify(bgm_id)}"
    self << %(bgm c_bgm_folder + "\\" + $i2 + ".wav")
    self << "^Playing BGM '^$i3^'"
  end

  def ins_0x91(val1)
    nyi
    debug "instruction 0x91, val1: #{hex(val1)}"
  end

  def ins_0x92(val1, val2)
    nyi
    debug "instruction 0x92, val1: #{val1}, val2: #{val2}"
  end

  def ins_0x94(val1)
    nyi
    debug "instruction 0x94, val1: #{hex(val1)}"
  end

  def play_se(channel, se_id, fadein_frames, loop_flag, volume, val4, val5)
    # loop_flag: 0 = looping, 1 = play once
    debug "Play sound effect, channel: #{channel}, se_id: #{se_id}, fadein_frames: #{fadein_frames}, loop_flag: #{loop_flag}, volume: #{volume}, val4: #{val4}, val5: #{val5}"
    # TODO: fadein
    self << "chvol #{nscify(channel)}, #{nscify(volume)}"
    self << "#{LookupTable.for("se")} #{nscify(se_id)}"
    self << %(if #{nscify(loop_flag)} > 0: dwave #{nscify(channel)}, c_se_folder + "\\" + $i2 + ".wav")
    self << %(if #{nscify(loop_flag)} <= 0: dwaveloop #{nscify(channel)}, c_se_folder + "\\" + $i2 + ".wav")
  end

  def fadeout_se(channel, duration)
    debug "Sound effect fadeout, channel: #{channel}, duration: #{duration}"
    self << "dwavestop #{nscify(channel)}"
  end

  def ins_0x97(channel)
    nyi
    debug "instruction 0x97 (bgm related?), channel: #{hex(channel)}"
  end

  def ins_0x98(val1, val2, val3)
    nyi
    debug "instruction 0x98, val1: #{val1}, val2: #{val2}, val3: #{val3}"
  end

  def ins_0x9a(val1, val2)
    nyi
    debug "instruction 0x9a (sound related?), val1: #{val1}, val2: #{val2}"
  end

  def ins_0x9b(val1, val2, val3, val4, val5)
    nyi
    debug "instruction 0x9b (rumble?), val1: #{val1}, val2: #{val2}, val3: #{val3}, val4: #{val4}, val5: #{val5}"
  end

  def play_voice(name, volume, val2)
    debug "play voice, name: '#{name}', volume: #{volume}, val2: #{val2}"
    self << "voicevol #{volume}"
    self << %(wave "voice/#{name}.wav")
  end

  def ins_0x9e(val1)
    nyi
    debug "instruction 0x9e (volume related?), val1: #{val1}"
  end

  def ins_0x9f(val1, val2)
    nyi
    debug "instruction 0x9f, val1: #{val1}, val2: #{val2}"
  end

  # Sections, timers

  def section_title(type, str)
    nyi
    debug "section title: '#{str}', type: #{hex(type)}"
  end

  def ins_0xa1
    nyi
    debug "instruction 0xa1 (set timer?)"
  end

  def ins_0xa2(argument)
    nyi
    debug "instruction 0xa1 (clear timer & disable skip?), argument: #{hex(argument)}"
  end

  def ins_0xa3
    nyi
    debug "instruction 0xa3 (unset timer?)"
  end

  def ins_0xa6(val1, val2)
    nyi
    debug "instruction 0xa6, val1: #{val1}, val2: #{val2}"
  end

  def ins_0xb0(val)
    nyi
    debug "instruction 0xb0 (marker?), val: #{hex(val)}"
  end

  def ins_0xb1(val1, data)
    nyi
    debug "instruction 0xb1, val1: #{val1}, data: #{data}"
  end

  # Game specific instructions

  def ins_0xe0_kal(data)
    nyi
    debug "instruction 0xe0 (Kal specific), data: #{data}"
  end

  def ins_0xe0_saku(data) # related to updating the character screen
    nyi
    debug "instruction 0xe0 (Saku specific), data: #{data}"
  end

  def ins_0xe1_saku(data)
    nyi
    debug "instruction 0xe1 (Saku specific), data: #{data}"
  end

  def ins_0xe2_saku(data)
    nyi
    debug "instruction 0xe2 (Saku specific), data: #{data}"
  end

  def ins_0xe3_saku(data)
    nyi
    debug "instruction 0xe3 (Saku specific), data: #{data}"
  end

  def ins_0xe4_saku(data) # related to updating the character screen
    nyi
    debug "instruction 0xe4 (Saku specific), data: #{data}"
  end
end

# Generates a number-to-string lookup table in NScripter code. Used to access assets by their IDs.
# This is done by essentially using a really big tablegoto, with each target doing a mov and then return
# TODO: there may be a much more elegant method to do this
class LookupTable
  def initialize(name)
    @name = name
    @elements = []
    @current_index = 0
  end

  # Appends some data.
  # Data should be in the format [[$i2, "blah"], [%i3, 12345], ...]
  def append(id, data)
    raise "Invalid ID" if id != @current_index
    @elements << data
    @current_index += 1
  end

  def generate
    return "\n" if @elements.empty?
    str = StringIO.new
    str.puts "*#{LookupTable.for(@name)}"
    str.puts "getparam %i1"
    str.puts "tablegoto %i1, " + @elements.map.with_index { |e, i| "*#{entry_for(i)}" }.join(', ')
    str.puts "return"
    @elements.each_with_index do |e, i|
      str.write "*#{entry_for(i)}:"
      e.each do |target, value|
        str.write "mov #{target}, #{value}:"
      end
      str.puts "return"
    end
    str.puts
    str.string
  end

  def write_to(out)
    orig = out.offset
    out.offset = 1
    out << "defsub #{LookupTable.for(@name)}"
    out.offset = out.script_offset
    out << generate
    out.offset = orig
  end

  def self.for(name); "lt_lookup_#{name}"; end
  def entry_for(id); "lt_#{@name}_entry_#{id}"; end
end

# Parse file header
magic = file.read(4)
if magic != 'SNR '
  puts "Not an SNR file!".c(91)
  exit
end

filesize, dialogue_line_count, _, _, _, _, _ = file.unpack_read('L<L<L<L<L<L<L<')
script_offset, mask_offset, bg_offset, bustup_offset, bgm_offset, se_offset, movie_offset, voice_offset, offset8, offset9 = file.unpack_read('L<L<L<L<L<L<L<L<L<L<')

out = OutFile.new(0x0, script_offset)

out.offset = 0
out.newline
out << "; Generated by Neurochitin's read_scenario.rb"
out << "; SHA256 checksum of original .snr file: #{sha256}"
out << "; Original filesize: #{filesize} bytes"
out << "; Number of dialogue lines: #{dialogue_line_count}"
out.newline

out.offset = 1
out << "; Functions"

out.offset = 2
out << "; Aliases for NScripter variables representing SNR registers"

out.offset = 3
out << "; Constants"
out << %(stralias c_bg_folder, "#{BG_FOLDER}")
out << %(stralias c_sprite_folder, "#{SPRITE_FOLDER}")
out << %(stralias c_bgm_folder, "#{BGM_FOLDER}")
out << %(stralias c_se_folder, "#{SE_FOLDER}")

# Read masks
Mask = Struct.new(:name, :offset)
file.read_table(mask_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read(MODE == :konosuba ? 'C' : 'S<') # Konosuba appears to only use one byte for many string lengths
  name = file.read_shift_jis(len)
  out.masks[n] = Mask.new(name, file.pos)
  out << "; Mask 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name}"
end
out.newline

# Read backgrounds
lut = LookupTable.new("background")
Background = Struct.new(:name, :offset, :val1)
file.read_table(bg_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read(MODE == :konosuba ? 'C' : 'S<')
  name = file.read_shift_jis(len)
  name.gsub!("%TIME%", "a") if MODE == :kal # TODO: find out what's up with these %TIME% bgs
  val1, _ = file.unpack_read('S<')
  out.backgrounds[n] = Background.new(name, file.pos, val1)

  lut.append(n, [["$i2", out.raw_background(n)]])

  out << "; Background 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name} (val1 = 0x#{val1.to_s(16)})"
  out << %(stralias #{out.raw_background(n)}, "#{File.join(BG_FOLDER, name + BG_EXT)}")
end
out.newline
lut.write_to(out)
puts "Read #{out.backgrounds.length} backgrounds"

# Read bustups
lut = LookupTable.new("bustup")

if MODE == :kal
  # In Kal, bustups only have one name, but a bunch of values at the end.
  Bustup = Struct.new(:name, :offset, :val1, :val2, :val3, :val4)
else
  # In Saku, bustups have two strings (what I assume are name and expression), and only one value at the end.
  Bustup = Struct.new(:name, :expression, :offset, :val1)
end

file.read_table(bustup_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read(MODE == :konosuba ? 'C' : 'S<')
  if MODE == :kal
    name = file.read_shift_jis(len)
    name.gsub!("%DRESS%", "首輪")
    val1, val2, val3, val4 = file.unpack_read('S<S<S<s<')
    out.bustups[n] = Bustup.new(name, file.pos, val1, val2, val3, val4)

    lut_entry = []
    lut_entry << ["$i2", out.raw_bustup(n)]
    # potentially add more data here? like offsets etc.
    lut.append(n, lut_entry)
    out << "; Bustup 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name} (val1 = 0x#{val1.to_s(16)}, val2 = 0x#{val2.to_s(16)}, val3 = 0x#{val3.to_s(16)}, val4 = 0x#{val4.to_s(16)})"
  else
    name = file.read_shift_jis(len)
    len, _ = file.unpack_read(MODE == :konosuba ? 'C' : 'S<')
    expr = file.read_shift_jis(len)
    val1, _ = file.unpack_read('S<')
    out.bustups[n] = Bustup.new(name, expr, file.pos, val1)

    lut_entry = []
    lut_entry << ["$i2", out.raw_bustup(n)]
    lut_entry << ["$i3", %("#{expr}")]
    # potentially add more data here? like offsets etc.
    lut.append(n, lut_entry)
    out << "; Bustup 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name} #{expr} (val1 = 0x#{val1.to_s(16)})"
  end
  out << %(stralias #{out.raw_bustup(n)}, "#{name}")
end
out.newline
lut.write_to(out)
puts "Read #{out.bustups.length} bustups"

# Read BGM
lut = LookupTable.new("bgm")
BGMTrack = Struct.new(:name1, :name2, :offset, :val1)
file.read_table(bgm_offset) do |n|
  out.offset = file.pos
  len1, _ = file.unpack_read('S<')
  name1 = file.read_shift_jis(len1)
  len2, _ = file.unpack_read('S<')
  name2 = file.read_shift_jis(len2)
  val1, _ = file.unpack_read('S<')
  out.bgm_tracks[n] = BGMTrack.new(name1, name2, file.pos, val1)

  lut_entry = []
  lut_entry << ["$i2", out.raw_bgm_track(n)]
  lut_entry << ["$i3", %("#{name2}")]
  lut.append(n, lut_entry)

  out << "; BGM 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name1} #{name2} (val1 = 0x#{val1.to_s(16)})"
  out << %(stralias #{out.raw_bgm_track(n)}, "#{name1}" ; #{name2})
end
out.newline
lut.write_to(out)
puts "Read #{out.bgm_tracks.length} BGM tracks"

# Read SFX
lut = LookupTable.new("se")
SoundEffect = Struct.new(:name, :offset)
file.read_table(se_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read(MODE == :konosuba ? 'C' : 'S<')
  name = file.read_shift_jis(len)
  out.sound_effects[n] = SoundEffect.new(name, file.pos)
  lut.append(n, [["$i2", out.raw_sound_effect(n)]])
  out << "; Sound effect 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name}"
  out << %(stralias #{out.raw_sound_effect(n)}, "#{name}")
end
out.newline
lut.write_to(out)
puts "Read #{out.sound_effects.length} sound effects"

# Read movies
Movie = Struct.new(:name, :offset, :val1, :val2, :val3)
file.read_table(movie_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read(MODE == :konosuba ? 'C' : 'S<')
  name = file.read_shift_jis(len)
  val1, val2, val3 = file.unpack_read(MODE == :konosuba ? 'S<S<' : 'S<S<S<')
  out.movies[n] = Movie.new(name, file.pos, val1, val2, val3)
  out << "; Movie 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name} (val1 = 0x#{val1.to_s(16)}, val2 = 0x#{val2.to_s(16)}, val3 = 0x#{val3&.to_s(16)})"
end
out.newline
puts "Read #{out.movies.length} movies"

# Read voices
Voice = Struct.new(:name, :offset, :values)
file.read_table(voice_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read(MODE == :konosuba ? 'C' : 'S<')
  name = file.read_shift_jis(len)
  if MODE == :kal
    # Kal always has two values here.
    values = file.unpack_read('CC')
  else
    # Saku has a number of values prefixed with their length.
    len, _ = file.unpack_read('C')
    values = file.unpack_read('C' * len)
  end
  out.voices[n] = Voice.new(name, file.pos, values)
  out << "; Voice 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name} (values = #{values.map { |e| e.to_s(16) }.join(' ') })"
end
out.newline
puts "Read #{out.voices.length} voices"

# Read ???
Table8Entry = Struct.new(:name, :offset, :data)
file.read_table(offset8, length_prefix = false) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read('S<')
  name = file.read_shift_jis(len)
  len2, _ = file.unpack_read('S<')
  data = file.unpack_read('S<' * len2)
  out.table8[n] = Table8Entry.new(name, file.pos, data)
  out << "; table 8 entry 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name} (data: #{data.map { |e| e.to_s(16) } })"
end
out.newline
puts "Read #{out.table8.length} table 8 entries"

# Read ?????
Table9Entry = Struct.new(:offset, :val1, :val2, :val3)
file.read_table(offset9, length_prefix = false) do |n|
  out.offset = file.pos
  val1, val2, val3 = file.unpack_read('S<S<S<')
  out.table9[n] = Table9Entry.new(file.pos, val1, val2, val3)
  out << "; table 9 entry 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: val1 = 0x#{val1.to_s(16)}, val2 = 0x#{val2.to_s(16)}, val3 = 0x#{val3.to_s(16)}"
end
puts "Read #{out.table9.length} table 9 entries"

out.newline

# Parse script header
file.seek(script_offset)
out.offset = script_offset
section_size, element_count = file.unpack_read('L<L<')
out << "; Starting script section"
out << "; Script section size: #{section_size}"
out << "; Element count: #{element_count}"
out << "*snr_script_start"
out.newline

# The loop for parsing the script data
while true do
  out.offset = file.pos
  begin
    instruction = file.readbyte
  rescue EOFError
    puts "Done parsing!"
    break
  end
  puts "#{(file.pos - 1).to_s(16)} Instruction: 0x#{instruction.to_s(16)}"

  case instruction
  when 0x00 # exit?
    out.end
  when 0x40 # ????
    val1, val2, val3 = file.read_variable_length(3)
    out.ins_0x40(val1, val2, val3)
  when 0x41 # Modify register (very sure about this)
    mode, register = file.unpack_read('CS<')
    data1, _ = file.read_variable_length(1)
    case mode
    when 0x00 # signed assignment?
      out.register_signed_assign(register, data1)
    when 0x01
      out.register_unsigned_assign(register, data1)
    when 0x02
      out.register_add(register, data1)
    when 0x03
      out.register_sub(register, data1)
    when 0x04 # multiplication?
      out.register_mul(register, data1)
    when 0x05
      out.register_div(register, data1)
    when 0x06
      out.register_or(register, data1)
    when 0x07
      out.register_and(register, data1)
    when 0x08 # ?
      out.register_0x08(register, data1)
    when 0x82 # two-argument addition?
      data2, _ = file.read_variable_length(1)
      out.register_add2(register, data1, data2)
    when 0x83 # two-argument subtraction (register = data1 - data2)
      data2, _ = file.read_variable_length(1)
      out.register_sub2(register, data1, data2)
    when 0x84 # ? two_argument multiplication?
      data2, _ = file.read_variable_length(1)
      out.register_0x84(register, data1, data2)
    when 0x85 # ? two_argument division?
      data2, _ = file.read_variable_length(1)
      out.register_0x85(register, data1, data2)
    when 0x86 # ?
      data2, _ = file.read_variable_length(1)
      out.register_0x86(register, data1, data2)
    when 0x87 # ?
      data2, _ = file.read_variable_length(1)
      out.register_0x87(register, data1, data2)
    else
      puts "Unknown modify register mode"
      break
    end
  when 0x42 # calculation
    target, _ = file.unpack_read('S<')
    operations = []
    loop do
      byte = file.readbyte
      byte_print([byte], 96)
      case byte
      when 0xff
        break
      when 0x00 # push
        to_push, _ = file.read_variable_length(1)
        operations << [byte, to_push]
      else
        operations << [byte]
      end
    end

    out.calc(target, operations)
  when 0x43 # ??
    val1, _ = file.read_variable_length(1)
    length, _ = file.unpack_read('S<')
    data = file.unpack_read('S<' * length)
    out.store_in_multiple_registers(val1, data)
  when 0x44 # ??
    register, _ = file.unpack_read('S<')
    val3, _ = file.read_variable_length(1)
    len, _ = file.unpack_read('S<')
    data = []
    len.times do
      val, _ = file.read_variable_length(1)
      file.read(4 - val.length)
      data << val
    end
    # data = file.unpack_read('L<' * len)
    out.lookup_read(register, val3, data)
  when 0x45 # ??
    val1, val2 = file.read_variable_length(2)
    length, _ = file.unpack_read('S<')
    data = file.unpack_read('S<' * length)
    out.lookup_store(val1, val2, data)
  when 0x46 # conditional jump
    comparison_mode, _ = file.unpack_read('C')
    case comparison_mode
    when 0x00
      val1, val2 = file.read_variable_length(2)
      address, _ = file.unpack_read('L<')
      out.conditional_jump_equal(val1, val2, address)
    when 0x01
      val1, val2 = file.read_variable_length(2)
      address, _ = file.unpack_read('L<')
      out.conditional_jump_inequal(val1, val2, address)
    when 0x02
      val1, val2 = file.read_variable_length(2)
      address, _ = file.unpack_read('L<')
      out.conditional_jump_greater_or_equal(val1, val2, address)
    when 0x03
      val1, val2 = file.read_variable_length(2)
      address, _ = file.unpack_read('L<')
      out.conditional_jump_greater_than(val1, val2, address)
    when 0x04
      val1, val2 = file.read_variable_length(2)
      address, _ = file.unpack_read('L<')
      out.conditional_jump_less_or_equal(val1, val2, address)
    when 0x05
      val1, val2 = file.read_variable_length(2)
      address, _ = file.unpack_read('L<')
      out.conditional_jump_less_than(val1, val2, address)
    when 0x06
      val1, val2 = file.read_variable_length(2)
      address, _ = file.unpack_read('L<')
      out.conditional_jump_0x06(val1, val2, address)
    when 0x86
      val1, val2 = file.read_variable_length(2)
      address, _ = file.unpack_read('L<')
      out.conditional_jump_0x86(val1, val2, address)
    else
      puts "Unknown comparison mode"
      break
    end
  when 0x47 # jump to address unconditionally ?
    address, _ = file.unpack_read('L<')
    out.unconditional_jump(address)
  when 0x48 # gosub?
    address, _ = file.unpack_read('L<')
    out.ins_0x48(address)
  when 0x49 # return?
    out.ins_0x49
  when 0x4a # jump on value?
    value, _ = file.read_variable_length(1)
    len, _ = file.unpack_read('S<')
    targets = file.unpack_read('L<' * len)
    out.table_goto(value, targets)
  when 0x4b # another kind of jump on value
    # register is likely related to val1 from 0x46 0x00
    register, len = file.unpack_read('CS<')
    targets = file.unpack_read('L<' * len)
    out.ins_0x4b(register, targets)
  when 0x4c # ??
    data = file.unpack_read('CCCC')
    out.ins_0x4c(data)
  when 0x4d # maybe stack push?
    len, _ = file.unpack_read('C')
    values = file.read_variable_length(len)
    out.stack_push(values)
  when 0x4e # maybe stack pop?
    len, _ = file.unpack_read('C')
    values = file.unpack_read('S<' * len)
    out.stack_pop(values)
  when 0x4f # function call
    address, len = file.unpack_read('L<C')
    puts "Greater than 0xffff!" if address > 0xffff
    data = file.read_variable_length(len)
    out.call(address, data)
  when 0x50 # return from function called with 0x4f
    out.return
  when 0x51 # matching to values?
    reg, _ = file.unpack_read('S<')
    val3, val4 = file.read_variable_length(2)
    length, _ = file.unpack_read('S<')
    data = file.unpack_read('L<' * length)
    out.ins_0x51(reg, val3, val4, data)
  when 0x52 # ?? maybe some kind of return?
    out.ins_0x52
  when 0x53 # only used in konosuba
    reg, _ = file.unpack_read('S<')
    val1, val2 = file.read_variable_length(2)
    out.ins_0x53(reg, val1, val2)
  when 0x80
    reg, _ = file.unpack_read('S<')
    val1, _ = file.read_variable_length(1)
    out.ins_0x80(reg, val1)
  when 0x81 # only used in saku, maybe "read external" (chiru)?
    register, _ = file.unpack_read('S<')
    val1, _ = file.read_variable_length(1)
    out.ins_0x81(register, val1)
  when 0x82 # ????????
    data = file.unpack_read('C' * 2)
    out.ins_0x82(data)
  when 0x83 # ??
    mode, _ = file.unpack_read('C')
    val, _ = file.read_variable_length(1)
    out.wait_frames(mode, val)
  when 0x85 # ??
    val1, _ = file.read_variable_length(1)
    out.ins_0x85(val1)
  when 0x86 # dialogue
    if MODE == :saku
      # Saku somehow stores the dialogue number in a three-byte integer??
      dialogue_num_low_bytes, dialogue_num_high_byte, var1, length = file.unpack_read('S<CCS<')
      dialogue_num = (dialogue_num_high_byte << 16) | dialogue_num_low_bytes
    else
      dialogue_num, var1, length = file.unpack_read('L<CS<')
    end
    str = file.read_shift_jis(length)
    out.dialogue(dialogue_num, var1, length, str)
    break if dialogue_num > max_dialogue
  when 0x87 # dialogue pipe wait
    argument, _ = file.unpack_read('C')
    puts "Not 0x7F!" if argument != 0x7f
    out.ins_0x87(argument)
  when 0x88 # only used in saku
    out.ins_0x88
  when 0x89 # hide dialogue window?
    argument, _ = file.unpack_read('C')
    puts "Not 0x00!" if argument != 0x00
    val1, _ = file.read_variable_length(1)
    out.ins_0x89(argument, val1)
  when 0x8a # ??
    argument, _ = file.unpack_read('C')
    puts "Not 0x01!" if argument != 0x01
    out.ins_0x8a(argument)
  when 0x8b # ??
    data = file.unpack_read('CCCCC')
    out.ins_0x8b(data)
  when 0x8d # some kind of internal call
    val1, val2, register = file.unpack_read('S<S<S<')
    val3, _ = file.read_variable_length(1)
    len, _ = file.unpack_read('S<')
    code = file.read_shift_jis(len)
    len, _ = file.unpack_read('S<')
    data = file.read_shift_jis(len)
    out.ins_0x8d(val1, val2, register, val3, code, data)
  when 0x8e # MAYBE something related to printing?
    val1, val2, val3 = file.read_variable_length(3)
    length_byte, _ = file.unpack_read('C')

    # Count the number of 1 bits in length_byte. Having this be the length
    # explains all instances of 0x8e I've encountered so far, but I have
    # absolutely no idea why it would be this way.
    length = 0
    while length_byte > 0; length_byte &= length_byte - 1; length += 1; end

    data = file.read_variable_length(length)
    out.ins_0x8e(val1, val2, val3, data)
  when 0x8f # ??
    out.ins_0x8f
  when 0x90 # ??
    val1, val2, val3, val4 = file.read_variable_length(4)
    out.play_bgm(val1, val2, val3, val4)
  when 0x91 # ??
    val1, _ = file.read_variable_length(1)
    out.ins_0x91(val1)
  when 0x92 # ??
    val1, val2 = file.read_variable_length(2)
    out.ins_0x92(val1, val2)
  when 0x94 # only used in saku
    val1, _ = file.read_variable_length(1)
    out.ins_0x94(val1)
  when 0x95 # sfx related?
    # all of these are hypothetical...
    channel, sfxid, val1, val2, val3, val4, val5 = file.read_variable_length(7)
    out.play_se(channel, sfxid, val1, val2, val3, val4, val5)
  when 0x96 # also sfx related? some kind of fade?
    channel, _ = file.read_variable_length(1)
    var1, _ = file.read_variable_length(1)
    out.fadeout_se(channel, var1)
  when 0x97 # ?? BGM related?
    argument, _ = file.read_variable_length(1)
    out.ins_0x97(argument)
  when 0x98 # ??
    val1, val2, val3 = file.read_variable_length(3)
    out.ins_0x98(val1, val2, val3)
  when 0x9a # sound related?
    val1, val2 = file.read_variable_length(2)
    out.ins_0x9a(val1, val2)
  when 0x9b # rumble?
    val1, val2, val3, val4, val5 = file.read_variable_length(5)
    out.ins_0x9b(val1, val2, val3, val4, val5)
  when 0x9c # only used in saku, *probably* plays a voice independent from dialogue?
    len, _ = file.unpack_read('S<')
    str = file.read_shift_jis(len) # example of such a string: "02/10800000", which references a voice file where Krauss says "otousan! otousan!
    val1, val2 = file.read_variable_length(2) # val1 is probably a volume
    out.play_voice(str, val1, val2)
  when 0x9e # only used in saku
    argument, _ = file.read_variable_length(1)
    out.ins_0x9e(argument)
  when 0x9f # ??
    val1, val2 = file.read_variable_length(2)
    out.ins_0x9f(val1, val2)
  when 0xa0 # section title
    type, length = file.unpack_read('CS<')
    str = file.read_shift_jis(length)
    out.section_title(type, str)
  when 0xa1 # set timer?
    out.ins_0xa1
  when 0xa2 # clear timer and disable skip??
    argument, _ = file.read_variable_length(1)
    out.ins_0xa2(argument)
  when 0xa3 # unset timer?
    out.ins_0xa3
  when 0xa6 # only used in saku
    val1, val2 = file.read_variable_length(2)
    out.ins_0xa6(val1, val2)
  when 0xb0 # section marker?
    val, _ = file.unpack_read('C')
    out.ins_0xb0(val)
  when 0xb1 # only used in saku
    val1, len = file.unpack_read('CC')
    data = file.read_variable_length(len)
    out.ins_0xb1(val1, data)
  when 0xc0
    slot, _ = file.read_variable_length(1)
    out.ins_0xc0(slot)
  when 0xc1 # resource command?
    slot, _ = file.read_variable_length(1)
    command, _ = file.unpack_read('C')
    case command
    when 0x0
      val1, val2 = file.read_variable_length(2)
      out.resource_command_0x0(slot, val1, val2)
    when 0x1 # load simple?
      val1, val2, val3, val4, val5, val6, val7 = file.read_variable_length(7)
      out.resource_command_0x1(slot, val1, val2, val3, val4, val5, val6, val7)
    when 0x2
      val1, _ = file.read_variable_length(1)
      mode, _ = file.unpack_read('C')
      case mode
      when 0x0 # only used in saku
        out.load_background_0x0(slot, val1)
      when 0x1 # used in kal most of the time
        val2, _ = file.read_variable_length(1)
        out.load_background_0x1(slot, val1, val2)
      when 0x3 # used once in kal
        val2, val3 = file.read_variable_length(2)
        out.load_background_0x3(slot, val1, val2, val3)
      else
        raise "Invalid 0xc1 0x2 mode: 0x#{mode.to_s(16)}"
      end
    when 0x3
      val1, _ = file.read_variable_length(1)
      mode, _ = file.unpack_read('C')
      if mode == 0x1
        # Appears to be a simpler mode, only used during script tests in kal.
        val3, _ = file.read_variable_length(1)
        out.load_sprite(slot, val1, mode, val3, nil, nil, nil)
      elsif mode == 0xf # The mode actually used to load sprites in kal
        val3, val4, val5, val6 = file.read_variable_length(4)
        out.load_sprite(slot, val1, mode, val3, val4, val5, val6)
      elsif mode == 0x0 # Used in saku
        out.load_sprite(slot, val1, mode, nil, nil, nil, nil)
      elsif mode == 0x3 # Used in konosuba
        val3, val4 = file.read_variable_length(2)
        out.load_sprite(slot, val1, mode, val3, val4, nil, nil)
      else
        raise "Invalid 0xc1 0x3 mode: 0x#{mode.to_s(16)}"
      end
    when 0x4
      val1, val2 = file.read_variable_length(2)
      out.resource_command_0x4(slot, val1, val2)
    when 0x6
      val1, val2 = file.unpack_read('CC')
      case val2
      when 0x01
        data, _ = file.read_variable_length(1)
        out.resource_command_0x6_0x1(slot, val1, data)
      when 0x02
        data, _ = file.read_variable_length(1)
        out.resource_command_0x6_0x2(slot, val1, data)
      when 0x03
        val3, val4 = file.read_variable_length(2)
        out.resource_command_0x6_0x3(slot, val1, val3, val4)
      when 0x05
        val3, val4 = file.read_variable_length(2)
        out.resource_command_0x6_0x5(slot, val1, val3, val4)
      else
        puts "Unknown resource command 0x06 flag"
        break
      end
    when 0x8 # only used in saku
      val1, val2, val3, val4 = file.read_variable_length(4)
      out.resource_command_0x8(slot, val1, val2, val3, val4)
    when 0x9
      val1, val2, val3 = file.read_variable_length(3)
      out.resource_command_0x9(slot, val1, val2, val3)
    else
      puts "Unknown resource command"
      break
    end
  when 0xc2 # sprite command
    slot, _ = file.read_variable_length(1)
    command, _ = file.unpack_read('C')
    case command
    when 0x00
      out.sprite_command_hide(slot)
    when 0x01
      out.sprite_command_0x01(slot)
    when 0x12
      out.sprite_command_0x12(slot)
    else
      puts "Unknown sprite command"
      break
    end
  when 0xc3 # sprite wait (pretty sure)
    val1, val2 = file.read_variable_length(2)
    property, _ = file.unpack_read('C')
    case property
    when 0x00
      out.sprite_wait_0x00(val1, val2)
    when 0x01
      val3, _ = file.read_variable_length(1)
      out.sprite_set_basic_transform(val1, val2, val3)
    when 0x02
      val3, _ = file.read_variable_length(1)
      out.sprite_wait_0x02(val1, val2, val3)
    when 0x03
      val3, val4 = file.read_variable_length(2)
      out.sprite_wait_0x03(val1, val2, val3, val4)
    when 0x04
      val3, _ = file.read_variable_length(1)
      out.sprite_wait_0x04(val1, val2, val3)
    when 0x05
      val3, val4 = file.read_variable_length(2)
      out.sprite_wait_0x05(val1, val2, val3, val4)
    when 0x06
      val3, val4 = file.read_variable_length(2)
      out.sprite_wait_0x06(val1, val2, val3, val4)
    when 0x07
      val3, val4, val5 = file.read_variable_length(3)
      out.sprite_wait_0x07(val1, val2, val3, val4, val5)
    when 0x0f # anim?
      val3, val4, val5, val6 = file.read_variable_length(4)
      out.sprite_set_complex_transform(val1, val2, val3, val4, val5, val6)
    else
      puts "Unknown sprite wait property"
      break
    end
  when 0xc4 # ?
    target, _ = file.read_variable_length(1)
    length, _ = file.unpack_read('C')
    data = file.read_variable_length(length)
    out.ins_0xc4(target, data)
  when 0xc5 # konosuba only, probably does something similar as c6 in kal
    id, _ = file.read_variable_length(1)
    out.ins_0xc5(id)
  when 0xc6 # load sprite?
    #id1, id2 = file.unpack_read('CC')
    id1, id2 = file.read_variable_length(2)
    out.ins_0xc6(id1, id2)
  when 0xc7 # sprite command
    slot, _ = file.read_variable_length(1)
    command, _ = file.unpack_read('C')
    out.ins_0xc7(slot, command)
  when 0xc9 # kal only, has exactly the same syntax as 0x8e it seems
    val1, val2, val3 = file.read_variable_length(3)
    length_byte, _ = file.unpack_read('C')

    # Count the number of 1 bits in length_byte. Having this be the length
    # explains all instances of 0x8e/0xc9 I've encountered so far, but I have
    # absolutely no idea why it would be this way.
    length = 0
    while length_byte > 0; length_byte &= length_byte - 1; length += 1; end

    data = file.read_variable_length(length)
    out.ins_0xc9(val1, val2, val3, data)
  when 0xca # acts upon some register or something?
    register, _ = file.unpack_read('C')
    out.ins_0xca(register)
  when 0xcb # maybe waiting for something?
    out.ins_0xcb
  when 0xcc # special
    val1, _ = file.unpack_read('C')
    out.ins_0xcc(val1)
  when 0xcd
    out.ins_0xcd
  when 0xce
    val1, val2, val3 = file.read_variable_length(3)
    out.ins_0xce(val1, val2, val3)
  when 0xe1 # saku specific
    assert_mode :saku
    len, _ = file.unpack_read('C')
    data = file.unpack_read('C' * len)
    out.ins_0xe1_saku(data)
  when 0xe2 # saku specific
    assert_mode :saku
    data = file.read_variable_length(3)
    out.ins_0xe2_saku(data)
  when 0xe3 # saku specific
    assert_mode :saku
    data = file.read_variable_length(1)
    out.ins_0xe3_saku(data)
  when 0xe4 # saku specific
    assert_mode :saku
    data = file.read_variable_length(1)
    out.ins_0xe4_saku(data)
  when 0xff # another type of internal call
    length, _ = file.unpack_read('S<')
    code = file.read_shift_jis(length)
    argument_length, _ = file.unpack_read('C')
    arguments = file.read_variable_length(argument_length)
    out.ins_0xff(length, arguments)
  when 0xe0 # specific
    if MODE == :kal
      data = file.read_variable_length(3)
      out.ins_0xe0_kal(data)
    else
      assert_mode :saku
      data = file.read_variable_length(2)
      out.ins_0xe0_saku(data)
    end
  else
    puts "Unknown instruction"
    break
  end
end

puts "Writing..."

out.write(output_path) if output_path
File.write(dialogue_path, out.dialogue_lines.join("\n")) if dialogue_path
