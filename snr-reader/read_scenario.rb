require 'set'
require 'digest'
require 'stringio'
path = ARGV[0]

sha256 = Digest::SHA256.hexdigest(File.read(path))
puts sha256

if sha256 == '1a41c95be7427ddd3397249fde5be56dfd6f4a8cef20ab27a7a648f31e824dfb'
  load './assoc/kaleido.rb'
else
  ADDRESSES = {}
  REQUIRE_LABELS = Set.new
  puts "The sha256 of the loaded file does not match the expected value! You are probably trying to load a different SNR file. This may or may not work. Remove the exit statement from this check in the script to continue"
  #exit
end

file = open(path, 'rb')

# colourise string
class String; def c(a); "\e[#{a}m#{self}\e[0m"; end; end

LENS = {
  'c' => 1,
  's' => 2,
  'l' => 4,
  'q' => 8
}

CONVERTER = Encoding::Converter.new('SHIFT_JIS', 'UTF-8', invalid: :replace)

# Entergram uses halfwidth katakana instead of hiragana, probably to save a bit of space. We have to reverse this
HALFWIDTH = '｢｣ｧｨｩｪｫｬｭｮｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝｰｯ､ﾟﾞ･?｡'
HALFWIDTH_REPLACE = '「」ぁぃぅぇぉゃゅょあいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをんーっ、？！…　。'

# File references
BG_FOLDER = 'bg'
BG_EXT = '.png'

def byte_print(array, c = 94)
  Kernel.puts array.flatten.map { |e| e.to_s(16).rjust(2, '0') }.join(' ').c(c)
end

# represents a variable-length argument to an instruction
class Varlen
  attr_reader :data, :mode, :value

  def initialize(data)
    @data = data
    @first_byte = data.bytes[0]

    if @first_byte >= 0x80 && @first_byte <= 0x8f
      @mode = :m8
      @value = ((@first_byte & 0xF) << 8) | data.bytes[1]
    elsif @first_byte >= 0x90 && @first_byte <= 0x9f
      @mode = :m9
      # conjectured; I'm somewhat sure that this is big endian
      @value = ((@first_byte & 0xF) << 16) | (data.bytes[1] << 8) | data.bytes[2]
    elsif @first_byte == 0xc0
      @mode = :mc0 # most likely accesses registers > 16
      @value = data.bytes[1]
    elsif @first_byte >= 0xd0 && @first_byte <= 0xdf
      @mode = :md # most likely accesses function parameters
      @value = @first_byte & 0xf
    elsif @first_byte >= 0xe0 && @first_byte <= 0xef
      @mode = :me # most likely a sort of null value
      @value = 0
    elsif @first_byte >= 0xb0 && @first_byte <= 0xbf
      @mode = :mb # most likely accesses registers
      @value = @first_byte & 0xf
    elsif @first_byte >= 0x40 && @first_byte <= 0x7f
      @mode = :mneg # most likely negative values
      @value = @first_byte - 128
    else
      @mode = :mraw
      @value = @first_byte
    end

    # I am convinced that 0xa is a special mode, but it is not used anywhere
    # so I don't know what it does.
    # I am not entirely convinced that 0x40-0x7f are not also special modes,
    # because often e.g. 0x8050 is used in place of 0x50. But I do not know
    # what they would refer to.
  end

  def constant?
    [:mraw, :mneg, :m8, :m9].include? @mode
  end

  def length
    @data.length
  end

  def to_s
    hex = @data.bytes.map { |e| e.to_s(16).rjust(2, '0') }.join
    "V[#{@mode.to_s}, 0x#{hex}, #{@value}]"
  end
end

# Methods to make it easier to read certain things from the scenario file
class IO
  # Only supports C/c, S/s, L/l, Q/q at the moment
  def unpack_read(str)
    len = str.downcase.chars.map { |chr| LENS[chr] || 0 }.sum
    exit if len > 1000
    data = read(len)
    byte_print(data.bytes)
    result = data.unpack(str)
    p result
    result
  end

  def read_shift_jis(len)
    raw = read(len)
    raise 'Not null terminated!' unless raw.chars[-1] == 0.chr
    CONVERTER.convert(raw[0..-2]).tr(HALFWIDTH, HALFWIDTH_REPLACE)
  end

  def read_variable_length(len)
    # this is just for parsing. Notes for interpretation:
    # 8X YZ is *very* likely 0xXYZ
    # 9X XX XX = ?
    # A0..BF, C1..FF = registers?
    # C0 XX = ? (this one likely accesses memory or something)

    result = []
    len.times do
      first_byte = read(1)
      if (first_byte.bytes[0] >= 0x80 && first_byte.bytes[0] <= 0x9f) || first_byte.bytes[0] == 0xc0 #??
        second_byte = read(1)
        first_byte += second_byte
        if first_byte.bytes[0] >= 0x90 && first_byte.bytes[0] <= 0x9f #??
          third_byte = read(1)
          first_byte += third_byte
        end
      end
      result << Varlen.new(first_byte)
    end
    result.each { |v| Kernel.puts v.to_s.c(95) }
    result
  end

  def read_table(offset, length_prefix = true)
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
  attr_reader :offset, :script_offset

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
      @nsc_variable_counter += 1
      @known_registers << num
    end
    raw_register(num)
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
  # Otherwise it will return the closest NScripter equivalent to whatever the given val represents.
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

  # Remove characters that would not be allowed in nsc identifiers
  def normalize(str)
    norm = str.gsub(/[^A-Za-z0-9_]/, '')
    norm = "X#{norm}" if norm =~ /^[0-9_]/
    norm
  end

  def background(val)
    nscify(val) { |value| raw_background(value) }
  end

  def raw_background(id); "bg_0x#{id.to_s(16)}_#{normalize(@backgrounds[id].name)}"; end

  def null; "null"; end

  def nyi
    @nyi = true
  end

  # --------------------------------------- #
  # - methods for individual instructions - #
  # --------------------------------------- #

  def dialogue(num, var1, length, str)
    # 火凛@r@vkarin0002.｢ｺﾚッ､何だﾖﾟﾞ�ﾄ､取ﾚﾈｪッﾟﾞ｣
    # @rどｺｶ遠ｸでボイラｰが唸ｯﾃｲﾃ､ｿﾉ熱が伝ﾜｯﾃｸﾙ｡･･ｿﾝﾅ感じがｽﾙﾖｳﾅ､気配ﾅﾉだ｡
    character, line = str.split("@r")
    self << "; line #{num}, var1 #{var1}, spoken by #{character}"
    elements = line.split('.')
    self << "^#{character} ~y+10~" # character tag (temporary)
    self << "^#{elements.last}\\" # actual line
    self << "textclear"
    newline
  end

  def ins_0x87(argument)
    nyi
    debug "instruction 0x87 (dialogue pipe wait?), argument: #{hex(argument)}"
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

  def ins_0x8d(data)
    nyi
    debug "instruction 0x8d, argument: #{hex(data)}"
  end

  def ins_0x8e_0x2(var1, var2)
    nyi
    debug "instruction 0x8d, var1: #{hex(var1)}, var2: #{hex(var2)}"
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
    debug "mov #{register(reg)}, #{register(reg)} [OR] #{value}"
  end

  def register_and(reg, value)
    nyi
    debug "mov #{register(reg)}, #{register(reg)} [AND] #{value}"
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

  def register_0x84(reg, value)
    nyi
    debug "mov #{register(reg)}, #{register(reg)} [0x84] #{value}"
  end

  def register_0x87(reg, value1, value2)
    nyi
    debug "mov #{register(reg)}, #{value1} [0x87] #{value2}"
  end

  # Other register stuff

  def ins_0x40(val1, val2, val3)
    nyi
    debug "instruction 0x40, val1: #{val1}, val2: #{val2}, val3: #{val3}"
  end

  def calc(target, operations)
    stack = []
    operations.each do |op, val|
      case op
      when 0x00 # push
        stack.push(nscify(val))
      when 0x01
        second = stack.pop
        stack.push(['(', stack.pop, ' + ', second, ')'])
      when 0x02
        second = stack.pop
        stack.push(['(', stack.pop, ' - ', second, ')'])
      when 0x03
        second = stack.pop
        stack.push(['(', stack.pop, ' * ', second, ')'])
      when 0x04
        second = stack.pop
        stack.push(['(', stack.pop, ' / ', second, ')'])
      when 0x0b
        nyi
        stack.push(['([0x0b] ', stack.pop, ')'])
      when 0x18 # total conjecture: limiting operation?
        nyi
        third = stack.pop
        second = stack.pop
        stack.push(['(', stack.pop, ' [0x18] ', second, third, ')'])
      else
        nyi
        second = stack.pop
        stack.push(['(', stack.pop, " [0x#{op.to_s(16)}] ", second, ')'])
      end
    end

    if stack.length != 1
      nyi
      debug "Could not fully parse expression! Stack at the end: #{stack}"
    end

    self << "mov #{register(target)}, #{stack.flatten.join} ; calc #{operations.map { |e| e.length == 2 ? [hex(e.first), e.last.to_s] : [hex(e.first)] }.join(' ')}"
  end

  def ins_0x43(val1, data)
    nyi
    debug "instruction 0x43 (store multiple?), val1: #{val1}, data: #{hex(data)}"
  end

  def lookup(reg, val3, data)
    self << "movz ?lookup, #{data.map { |e| nscify(e) }.join(', ')} ; #{data.map(&:to_s)}"
    self << "mov #{register(reg)}, ?lookup[#{nscify(val3)}] ; #{val3}"
  end

  def ins_0x45(val1, val2, data)
    nyi
    debug "instruction 0x45 (store multiple?), val1: #{val1}, val2: #{val2}, data: #{hex(data)}"
  end

  # Control flow

  def call(addr, data)
    self << "#{address(addr)} #{data.map { |e| nscify(e) }.join(', ')} ; #{data.map(&:to_s).join(', ')}"
    unless @known_functions.include?(addr)
      @h[1] << "defsub #{address(addr)}"
      if data.length > 0
        lines_at_func = (@h[addr] ||= [])
        lines_at_func.unshift("getparam " + data.map.with_index { |_, i| parameter(i) }.join(", "))
      end
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
    self << "if #{nscify(val1)} & #{nscify(val2)} != 0 goto *#{address(addr)} ; #{val1} #{val2}"
  end

  def conditional_jump_0x86(val1, val2, addr)
    nyi; conditional_jump(val1, val2, addr, "[0x86]")
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
    self << "return ;0x#{@offset.to_s(16)}"
    newline
  end

  def ins_0x51(val1, val2, val3, val4, data)
    nyi
    debug "instruction 0x51, values: #{val1} #{val2} #{val3} #{val4}, data: #{data.map { |e| e.to_s(16) }}"
  end

  def ins_0x52
    nyi
    debug "instruction 0x52 (some kind of return?)"
  end

  def end
    nyi
    debug "end"
  end

  def syscall(target)
    nyi
    debug "syscall #{hex(target)}"
  end

  def ins_0x82(data)
    nyi
    debug "instruction 0x82: #{hex(data)}"
  end

  def ins_0x83(mode, val)
    nyi
    debug "instruction 0x83, mode: #{hex(mode)}, val: #{val}"
  end

  def ins_0x85(val1)
    nyi
    debug "instruction 0x85, val1: #{val1}"
  end

  # Stack related?

  def stack_push(values)
    values.each do |value|
      self << "mov %sp, %sp + 1:mov ?stack[%sp], #{nscify(value)} ; push #{value}"
    end
  end

  def stack_pop(values)
    values.each do |value|
      self << "mov #{register(value)}, ?stack[%sp]:mov %sp, %sp - 1 ; pop #{hex(value)}"
    end
  end

  def ins_0xff(data)
    nyi
    debug "instruction 0xff (data section?) at 0x#{@offset.to_s(16)}, data: #{data}"
  end

  # Sprites, resources

  def resource_command_0x0(slot, val1, val2)
    nyi
    debug "resource command (0xc1) 0x0 (remove slot?), slot #{slot}, values: #{val1} #{val2}"
  end

  def resource_command_0x1(slot, val1, val2, val3, val4, val5, width, height)
    nyi
    debug "resource command (0xc1) 0x1 (load simple?), slot #{slot}, values: #{val1} #{val2} #{val3} #{val4} #{val5} width: #{width} height: #{height}"
  end

  def resource_command_0x2(slot, val1, val2, picture_index)
    debug "resource command (0xc1) 0x2 (pic load?), slot #{slot}, values: #{val1} #{val2}, picture index: #{picture_index}"
    if picture_index.constant?
      self << "bg #{background(picture_index)}, 0"
    else
      self << "#{LookupTable.for("background")} #{nscify(picture_index)}"
      self << "bg $i2, 0"
    end
  end

  def resource_command_0x3(slot, val1, val2, val3)
    nyi
    debug "resource command (0xc1) 0x3 (sprite_load?), slot #{slot}, values: #{val1} #{val2} #{val3}"
  end

  def resource_command_0x4(slot, val1, val2)
    nyi
    debug "resource command (0xc1) 0x4 (anime_load?), slot #{slot}, values: #{val1} #{val2}"
  end

  def resource_command_0x6_0x2(slot, val1)
    nyi
    debug "resource command (0xc1) 0x6 0x2, slot #{slot}, values: #{val1}"
  end

  def resource_command_0x6_0x3(slot, val1, val3)
    nyi
    debug "resource command (0xc1) 0x6 0x3, slot #{slot}, values: #{val1} #{val3}"
  end

  def resource_command_0x9(slot, val1, val2, val3)
    nyi
    debug "resource command (0xc1) 0x9 (special?), slot #{slot}, values: #{val1} #{val2} #{val3}"
  end

  def sprite_command_0x00(slot)
    nyi
    debug "sprite command (0xc2) 0x00 (z order?)"
  end

  def sprite_command_0x01(slot)
    nyi
    debug "sprite command (0xc2) 0x01 (alpha?)"
  end

  def sprite_command_0x12(slot)
    nyi
    debug "sprite command (0xc2) 0x12 (y resize?)"
  end

  def sprite_wait_0x00(val1, val2)
    nyi
    debug "sprite wait (0xc3) 0x00, values: #{val1} #{val2}"
  end

  def sprite_wait_0x01(val1, val2, val3)
    nyi
    debug "sprite wait (0xc3) 0x01 (alpha?), values: #{val1} #{val2} #{val3}"
  end

  def sprite_wait_0x02(val1, val2, val3)
    nyi
    debug "sprite wait (0xc3) 0x02, values: #{val1} #{val2} #{val3}"
  end

  def sprite_wait_0x03(val1, val2, val3, val4)
    nyi
    debug "sprite wait (0xc3) 0x03, values: #{val1} #{val2} #{val3} #{val4}"
  end

  def sprite_wait_0x04(val1, val2, val3)
    nyi
    debug "sprite wait (0xc3) 0x04, values: #{val1} #{val2} #{val3}"
  end

  def sprite_wait_0x05(val1, val2, val3, val4)
    nyi
    debug "sprite wait (0xc3) 0x05 (x pos?), values: #{val1} #{val2} #{val3} #{val4}"
  end

  def sprite_wait_0x06(val1, val2, val3, val4)
    nyi
    debug "sprite wait (0xc3) 0x06 (y pos?), values: #{val1} #{val2} #{val3} #{val4}"
  end

  def sprite_wait_0x07(val1, val2, val3, val4, val5)
    nyi
    debug "sprite wait (0xc3) 0x07, values: #{val1} #{val2} #{val3} #{val4} #{val5}"
  end

  def sprite_wait_0x0f(val1, val2, val3, val4, val5, val6)
    nyi
    debug "sprite wait (0xc3) 0x0f, values: #{val1} #{val2} #{val3} #{val4} #{val5} #{val6}"
  end

  def ins_0xc0(slot)
    nyi
    debug "instruction 0xc0, slot: #{hex(slot)}"
  end

  def ins_0xc4(target, data)
    nyi
    debug "instruction 0xc0, target: #{target}, data: #{data}"
  end

  def ins_0xc6(id1, id2)
    nyi
    debug "instruction 0xc6 (load sprite?), id1: #{id1}, id2: #{id2}"
  end

  def ins_0xc7(slot, command)
    nyi
    debug "instruction 0xc7 (some sprite command?), slot: #{slot}, command: #{hex(command)}"
  end

  def ins_0xc9
    nyi
    debug "instruction 0xc9 (some kind of marker?)"
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

  def ins_0xcd(val1, val2)
    nyi
    debug "instruction 0xcd, val1: #{hex(val1)}, val2: #{hex(val2)}"
  end

  def ins_0xce(val1)
    nyi
    debug "instruction 0xce, val1: #{hex(val1)}"
  end

  def ins_0xd0; nyi; debug "instruction 0xd0 (special?)"; end
  def ins_0xd1; nyi; debug "instruction 0xd1 (special?)"; end
  def ins_0xd2; nyi; debug "instruction 0xd2 (special?)"; end
  def ins_0xd3; nyi; debug "instruction 0xd3 (special?)"; end
  def ins_0xd4; nyi; debug "instruction 0xd4 (special?)"; end

  def ins_0xe0(data)
    nyi
    debug "instruction 0xe0, data: #{data}"
  end

  # Sound related

  def ins_0x90(data)
    nyi
    debug "instruction 0x90, data: #{hex(data)}"
  end

  def ins_0x91(val1)
    nyi
    debug "instruction 0x91, val1: #{hex(val1)}"
  end

  def ins_0x92(val1)
    nyi
    debug "instruction 0x92, val1: #{hex(val1)}"
  end

  def ins_0x95(channel, sfxid, flag, val1, val2, val3)
    nyi
    debug "instruction 0x95 (sfx related?), channel: #{hex(channel)}, sfxid: #{hex(sfxid)}, flag: #{hex(flag)}, val1: #{val1}, val2: #{val2}, val3: #{val3}"
  end

  def ins_0x96(channel, val1)
    nyi
    debug "instruction 0x96 (sfx fade?), channel: #{hex(channel)}, val1: #{val1}"
  end

  def ins_0x97(channel)
    nyi
    debug "instruction 0x97 (bgm related?), channel: #{hex(channel)}"
  end

  def ins_0x98
    nyi
    debug "instruction 0x98"
  end

  def ins_0x9a(val1, val2)
    nyi
    debug "instruction 0x9a (sound related?), val1: #{val1}, val2: #{val2}"
  end

  def ins_0x9b(val1, val2, val3, val4, val5)
    nyi
    debug "instruction 0x9b (rumble?), val1: #{val1}, val2: #{val2}, val3: #{val3}, val4: #{val4}, val5: #{val5}"
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

  def ins_0xb0(val)
    nyi
    debug "instruction 0xb0 (marker?), val: #{hex(val)}"
  end

  def ins_0xb1(val1, data)
    nyi
    debug "instruction 0xb1, val1: #{hex(val1)}, data: #{hex(data)}"
  end

  def ins_0xb2(val)
    nyi
    debug "instruction 0xb2, val: #{hex(val)}"
  end

  def ins_0xbb(val)
    nyi
    debug "instruction 0xbb, val: #{hex(val)}"
  end

  def ins_0xb3; nyi; debug "instruction 0xb3"; end
  def ins_0xbd; nyi; debug "instruction 0xbd"; end
  def ins_0xbe; nyi; debug "instruction 0xbe"; end
  def ins_0xbf; nyi; debug "instruction 0xbf"; end
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

  def append(id, str)
    raise "Invalid ID" if id != @current_index
    @elements << str
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
      str.puts %(*#{entry_for(i)}:mov $i2, #{e}:return)
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

# How much dialogue to parse
max_dialogue = ARGV[2] ? ARGV[2].to_i : 100000000

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


# Read masks
Mask = Struct.new(:name, :offset)
file.read_table(mask_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read('S<')
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
  len, _ = file.unpack_read('S<')
  name = file.read_shift_jis(len)
  name.gsub!("%TIME%", "a") # TODO: find out what's up with these %TIME% bgs
  val1, _ = file.unpack_read('S<')
  out.backgrounds[n] = Background.new(name, file.pos, val1)
  lut.append(n, out.raw_background(n))
  out << "; Background 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name} (val1 = 0x#{val1.to_s(16)})"
  out << %(stralias #{out.raw_background(n)}, "#{File.join(BG_FOLDER, name + BG_EXT)}")
end
out.newline
lut.write_to(out)

# Read bustups
Bustup = Struct.new(:name, :offset, :val1, :val2, :val3, :val4)
file.read_table(bustup_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read('S<')
  name = file.read_shift_jis(len)
  val1, val2, val3, val4 = file.unpack_read('S<S<S<S<')
  out.bustups[n] = Bustup.new(name, file.pos, val1, val2, val3, val4)
  out << "; Bustup 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name} (val1 = 0x#{val1.to_s(16)}, val2 = 0x#{val2.to_s(16)}, val3 = 0x#{val3.to_s(16)}, val4 = 0x#{val4.to_s(16)})"
end
out.newline

# Read BGM
BGMTrack = Struct.new(:name1, :name2, :offset, :val1)
file.read_table(bgm_offset) do |n|
  out.offset = file.pos
  len1, _ = file.unpack_read('S<')
  name1 = file.read_shift_jis(len1)
  len2, _ = file.unpack_read('S<')
  name2 = file.read_shift_jis(len2)
  val1, _ = file.unpack_read('S<')
  out.bgm_tracks[n] = BGMTrack.new(name1, name2, file.pos, val1)
  out << "; BGM 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name1} #{name2} (val1 = 0x#{val1.to_s(16)})"
end
out.newline

# Read SFX
SoundEffect = Struct.new(:name, :offset)
file.read_table(se_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read('S<')
  name = file.read_shift_jis(len)
  out.sound_effects[n] = SoundEffect.new(name, file.pos)
  out << "; Sound effect 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name}"
end
out.newline

# Read movies
Movie = Struct.new(:name, :offset, :val1, :val2, :val3)
file.read_table(movie_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read('S<')
  name = file.read_shift_jis(len)
  val1, val2, val3 = file.unpack_read('S<S<S<')
  out.movies[n] = Movie.new(name, file.pos, val1, val2, val3)
  out << "; Movie 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name} (val1 = 0x#{val1.to_s(16)}, val2 = 0x#{val2.to_s(16)}, val3 = 0x#{val3.to_s(16)})"
end
out.newline

# Read voices
Voice = Struct.new(:name, :offset, :val1, :val2)
file.read_table(voice_offset) do |n|
  out.offset = file.pos
  len, _ = file.unpack_read('S<')
  name = file.read_shift_jis(len)
  val1, val2 = file.unpack_read('CC')
  out.voices[n] = Voice.new(name, file.pos, val1, val2)
  out << "; Voice 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: #{name} (val1 = 0x#{val1.to_s(16)}, val2 = 0x#{val2.to_s(16)})"
end
out.newline

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

# Read ?????
Table9Entry = Struct.new(:offset, :val1, :val2, :val3)
file.read_table(offset9, length_prefix = false) do |n|
  out.offset = file.pos
  val1, val2, val3 = file.unpack_read('S<S<S<')
  out.table9[n] = Table9Entry.new(file.pos, val1, val2, val3)
  out << "; table 9 entry 0x#{n.to_s(16)} at 0x#{file.pos.to_s(16)}: val1 = 0x#{val1.to_s(16)}, val2 = 0x#{val2.to_s(16)}, val3 = 0x#{val3.to_s(16)}"
end

out.newline

file.seek(script_offset)
out.offset = script_offset
section_size, element_count = file.unpack_read('L<L<')
out << "; Starting script section"
out << "; Script section size: #{section_size}"
out << "; Element count: #{element_count}"
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
  when 0x80 # syscall?
    target, _ = file.unpack_read('C')
    out.syscall(target)
  when 0x82 # ????????
    data = file.unpack_read('C' * 7)
    out.ins_0x82(data)
  when 0x83 # ??
    mode, _ = file.unpack_read('C')
    val, _ = file.read_variable_length(1)
    out.ins_0x83(mode, val)
  when 0x85 # ??
    val1, _ = file.unpack_read('C')
    out.ins_0x85(val1)
  when 0x86 # dialogue
    dialogue_num, var1, length = file.unpack_read('L<CS<')
    str = file.read_shift_jis(length)
    out.dialogue(dialogue_num, var1, length, str)
    break if dialogue_num > max_dialogue
  when 0x87 # dialogue pipe wait
    argument, _ = file.unpack_read('C')
    puts "Not 0x7F!" if argument != 0x7f
    out.ins_0x87(argument)
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
    #if data[1..-1] != [0x7f, 0x00, 0x00, 0x00]
    #  puts "Invalid 0x8b"
    #  break
    #end
    out.ins_0x8b(data)
  when 0x8d # ?????
    # only appears as:
    # 8D 00 00 00 00 01 00 90 0F
    # or
    # 8D 00 00 00 00 A2 00 90 0F

    val1, val2, val3 = file.unpack_read('L<S<S<')
    if val1 != 0 || val3 != 0x0f90
      puts "Invalid 8d"
      break
    end
    out.ins_0x8d(data)
  when 0x8e # MAYBE something related to printing?
    var1, flags = file.unpack_read('CC')
    case flags
    when 0x2
      var2, _ = file.unpack_read('S<')
      out.ins_0x8e_0x2(var1, var2)
    else
      puts "Invalid print flags"
      break
    end
  when 0x8f # ??
    out.ins_0x8f
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
      val1, val2, val3 = file.read_variable_length(3)
      out.resource_command_0x2(slot, val1, val2, val3)
    when 0x3
      val1, val2, val3 = file.read_variable_length(3)
      out.resource_command_0x3(slot, val1, val2, val3)
    when 0x4
      val1, val2 = file.read_variable_length(2)
      out.resource_command_0x4(slot, val1, val2)
    when 0x6
      val1, val2 = file.unpack_read('CC')
      case val2
      when 0x02
        data, _ = file.read_variable_length(1)
        out.resource_command_0x6_0x2(slot, val1)
      when 0x03
        val3, _ = file.unpack_read('C')
        data, _ = file.read_variable_length(1)
        out.resource_command_0x6_0x3(slot, val1, val3)
      else
        puts "Unknown resource command 0x06 flag"
        break
      end
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
      out.sprite_command_0x00(slot)
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
      out.sprite_wait_0x01(val1, val2, val3)
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
      out.sprite_wait_0x0f(val1, val2, val3, val4, val5, val6)
    else
      puts "Unknown sprite wait property"
      break
    end
  when 0xc4 # ?
    target, _ = file.read_variable_length(1)
    length, _ = file.unpack_read('C')
    data = file.read_variable_length(length)
    out.ins_0xc4(target, data)
  when 0xc6 # load sprite?
    #id1, id2 = file.unpack_read('CC')
    id1, id2 = file.read_variable_length(2)
    out.ins_0xc6(id1, id2)
  when 0xc7 # sprite command
    slot, _ = file.read_variable_length(1)
    command, _ = file.unpack_read('C')
    out.ins_0xc7(slot, command)
  when 0xc9 # some kind of marker?
    out.ins_0xc9
  when 0xca # acts upon some register or something?
    register, _ = file.unpack_read('C')
    out.ins_0xca(register)
  when 0xcb # maybe waiting for something?
    out.ins_0xcb
  when 0xcc # special
    val1, _ = file.unpack_read('C')
    out.ins_0xcc(val1)
  when 0xcd # special
    val1, val2 = file.unpack_read('CC')
    out.ins_0xcd(val1, val2)
  when 0xce
    val, _ = file.unpack_read('L<') # ?
    out.ins_0xce(val)
  when 0xd0 # probably special? d0-d4 often occur together
    out.ins_0xd0
  when 0xd1 # probably special? d0-d4 often occur together
    out.ins_0xd1
  when 0xd2 # probably special?
    out.ins_0xd2
  when 0xd3 # probably special?
    out.ins_0xd3
  when 0xd4 # probably special?
    out.ins_0xd4
  when 0xe0 # ????
    data = file.read_variable_length(3)
    out.ins_0xe0(data)
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
    when 0x84 # ?
      out.register_0x84(register, data1)
    when 0x87 # ?
      data2, _ = file.read_variable_length(1)
      out.register_0x87(register, data1, data2)
    else
      puts "Unknown modify register mode"
      break
    end
  when 0x4f # very likely calling a procedure
    address, len = file.unpack_read('L<C')
    puts "Greater than 0xffff!" if address > 0xffff
    data = file.read_variable_length(len)
    out.call(address, data)
  when 0x50 # ?? maybe some kind of return?
    out.return
  when 0x51 # complex return?
    val1, val2, val3, val4 = file.read_variable_length(4)
    length, _ = file.unpack_read('S<')
    addresses = file.unpack_read('L<' * length)
    out.ins_0x51(val1, val2, val3, val4, addresses)
  when 0x52 # ?? maybe some kind of return?
    out.ins_0x52
  when 0x90 # ??
    data = file.unpack_read('CCC')
    out.ins_0x90(data)
  when 0x91 # ??
    val1, _ = file.unpack_read('C')
    out.ins_0x91(val1)
  when 0x92 # ??
    val1, _ = file.unpack_read('C')
    out.ins_0x92(val1)
  when 0x95 # sfx related?
    # all of these are hypothetical...
    channel, sfxid, flag = file.unpack_read('CS<C')
    var1, var2, var3 = file.read_variable_length(3)
    out.ins_0x95(channel, sfxid, flag, var1, var2, var3)
  when 0x96 # also sfx related? some kind of fade?
    channel, _ = file.unpack_read('C')
    var1, _ = file.read_variable_length(1)
    out.ins_0x96(channel, var1)
  when 0x97 # ?? BGM related?
    argument, _ = file.unpack_read('C')
    out.ins_0x97(argument)
  when 0x98 # ??
    out.ins_0x98
  when 0x9a # sound related?
    val1, val2 = file.read_variable_length(2)
    out.ins_0x9a(val1, val2)
  when 0x9b # rumble?
    val1, val2, val3, val4, val5 = file.read_variable_length(5)
    out.ins_0x9b(val1, val2, val3, val4, val5)
  when 0xa0 # section title
    type, length = file.unpack_read('CS<')
    str = file.read_shift_jis(length)
    out.section_title(type, str)
  when 0xa1 # set timer?
    out.ins_0xa1
  when 0xa2 # clear timer and disable skip??
    argument, _ = file.unpack_read('C')
    out.ins_0xa2(argument)
  when 0xa3 # unset timer?
    out.ins_0xa3
  when 0xb0 # section marker?
    val, _ = file.unpack_read('C')
    out.ins_0xb0(val)
  when 0xb1 # ?
    val1, length = file.unpack_read('CS<')
    data = file.read(length)
    out.ins_0xb1(val1, data)
  when 0xb2 # ??
    val, _ = file.unpack_read('C')
    out.ins_0xb2(val)
  when 0xb3 # ??
    out.ins_0xb3
  when 0xbb # ??
    val, _ = file.unpack_read('C')
    out.ins_0xbb(val)
  when 0xbd # ??
    out.ins_0xbd
  when 0xbe # ??
    out.ins_0xbe
  when 0xbf # ??
    out.ins_0xbf
  when 0x40 # ????
    val1, val2, val3 = file.read_variable_length(3)
    out.ins_0x40(val1, val2, val3)
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
    val1, length = file.unpack_read('CS<')
    data = file.unpack_read('S<' * length)
    out.ins_0x43(val1, data)
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
    out.lookup(register, val3, data)
  when 0x45 # ??
    val1, val2 = file.read_variable_length(2)
    length, _ = file.unpack_read('S<')
    data = file.unpack_read('S<' * length)
    out.ins_0x45(val1, val2, data)
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
  when 0xff # some kind of data section?
    data = []
    loop do
      byte1 = file.read(1)
      puts byte1.ord
      break if byte1 == 0.chr
      byte2 = file.read(1)
      if byte2.ord >= 0x80
        # If the length "appears to" only be one byte, then some non-strings are stored and this is the end of the data section. I think my interpretation in this regard is wrong, but I can't come up with anything better.
        file.seek(-1, IO::SEEK_CUR)
        data << file.read_variable_length(byte1.ord)
        break
      else
        length, _ = (byte1 + byte2).unpack('S<')
        result = file.read_shift_jis(length)
        puts result
        data << result
        break if result.bytes.last == 0
      end
    end
    out.ins_0xff(data)
  else
    puts "Unknown instruction"
    break
  end
end

puts "Writing..."

out.write(ARGV[1]) if ARGV[1]
