# Tools that can be used to create SNR files

require 'stringio'

CONVERTER = Encoding::Converter.new('UTF-8', 'SHIFT_JIS', invalid: :replace)
HALFWIDTH = '｢｣ｧｨｩｪｫｬｭｮｱｲｳｴｵｶｷｸｹｺｻｼｽｾｿﾀﾁﾂﾃﾄﾅﾆﾇﾈﾉﾊﾋﾌﾍﾎﾏﾐﾑﾒﾓﾔﾕﾖﾗﾘﾙﾚﾛﾜｦﾝｰｯ､ﾟﾞ･?｡　'
HALFWIDTH_REPLACE = '「」ぁぃぅぇぉゃゅょあいうえおかきくけこさしすせそたちつてとなにぬねのはひふへほまみむめもやゆよらりるれろわをんーっ、？！…　。�'

class StringIO
  # Write length delimited SHIFT_JIS
  def write_str(str, halfwidth_replace = false)
    converted = KalSNRFile.to_shift_jis(str, halfwidth_replace)
    write([converted.bytes.length].pack('S<'))
    write(converted.bytes.pack('C*'))
  end
end

class KalSNRFile
  FILESIZE_LOCATION = 0x04
  DIALOGUE_LINE_COUNT_LOCATION = 0x08
  VAL1_LOCATION = 0x0c
  VAL2_LOCATION = 0x10
  VAL3_LOCATION = 0x14
  VAL4_LOCATION = 0x18
  VAL5_LOCATION = 0x1c
  SCRIPT_OFFSET_LOCATION = 0x20
  MASK_OFFSET_LOCATION = 0x24
  BG_OFFSET_LOCATION = 0x28
  BUSTUP_OFFSET_LOCATION = 0x2c
  BGM_OFFSET_LOCATION = 0x30
  SE_OFFSET_LOCATION = 0x34
  MOVIE_OFFSET_LOCATION = 0x38
  VOICE_OFFSET_LOCATION = 0x3c
  TABLE8_OFFSET_LOCATION = 0x40
  TABLE9_OFFSET_LOCATION = 0x44
  FIRST_TABLE = 0x54

  SCRIPT_MAGIC = 0xb00246

  def initialize(val1 = 0x1, val2 = 0x1)
    @data = StringIO.new
    @data.binmode

    @data.write('SNR ') # magic

    @data.seek(VAL1_LOCATION)
    @data.write([val1, val2].pack('L<L<'))

    @data.seek(FIRST_TABLE)

    @mask_num, @bg_num, @bustup_num, @bgm_num, @se_num, @movie_num, @voice_num, @table8_num, @table9_num = 0, 0, 0, 0, 0, 0, 0, 0, 0
    @masks, @bgs, @bustups, @bgms, @ses, @movies, @voices, @table8, @table9 = [], [], [], [], [], [], [], [], []
  end

  def write_to(file)
    File.write(file, data)
  end

  def data
    at(FILESIZE_LOCATION) { @data.write([@data.length].pack('L<'))}
    @data.string
  end

  def mask(*v)
    index = @masks.length
    @masks << v
    index
  end

  def write_masks
    o = write_table(@masks) do |s, mask|
      name, _ = mask
      s.write_str(name)
    end
    at(MASK_OFFSET_LOCATION) { @data.write([o].pack('L<'))}
  end

  def bg(*v)
    index = @bgs.length
    @bgs << v
    index
  end

  def write_bgs
    o = write_table(@bgs) do |s, bg|
      name, val1 = bg
      s.write_str(name)
      s.write([val1].pack('S<'))
    end
    at(BG_OFFSET_LOCATION) { @data.write([o].pack('L<'))}
  end

  def bustup(*v)
    index = @bustups.length
    @bustups << v
    index
  end

  def write_bustups
    o = write_table(@bustups) do |s, bustup|
      name, val1, val2, val3, val4 = bustup
      s.write_str(name)
      s.write([val1, val2, val3, val4].pack('S<S<S<s<'))
    end
    at(BUSTUP_OFFSET_LOCATION) { @data.write([o].pack('L<'))}
  end

  def bgm(*v)
    index = @bgms.length
    @bgms << v
    index
  end

  def write_bgms
    o = write_table(@bgms) do |s, bgm|
      name1, name2, val1 = bgm
      s.write_str(name1)
      s.write_str(name2)
      s.write([val1].pack('S<'))
    end
    at(BGM_OFFSET_LOCATION) { @data.write([o].pack('L<'))}
  end

  def se(*v)
    index = @ses.length
    @ses << v
    index
  end

  def write_ses
    o = write_table(@ses) do |s, se|
      name, _ = se
      s.write_str(name)
    end
    at(SE_OFFSET_LOCATION) { @data.write([o].pack('L<'))}
  end

  def movie(*v)
    index = @movies.length
    @movies << v
    index
  end

  def write_movies
    o = write_table(@movies) do |s, movie|
      name, val1, val2, val3 = movie
      s.write_str(name)
      s.write([val1, val2, val3].pack('S<S<S<'))
    end
    at(MOVIE_OFFSET_LOCATION) { @data.write([o].pack('L<'))}
  end

  def voice(*v)
    index = @voices.length
    @voices << v
    index
  end

  def write_voices
    o = write_table(@voices) do |s, voice|
      name, val1, val2 = voice
      s.write_str(name)
      s.write([val1, val2].pack('CC'))
    end
    at(VOICE_OFFSET_LOCATION) { @data.write([o].pack('L<'))}
  end

  def table8_entry(*v)
    index = @table8.length
    @table8 << v
    index
  end

  def write_table8
    o = write_table(@table8, length_prefix = false) do |s, entry|
      name, *data = entry
      s.write_str(name)
      s.write([data.length].pack('S<'))
      s.write(data.pack('S<' * data.length))
    end
    at(TABLE8_OFFSET_LOCATION) { @data.write([o].pack('L<'))}
  end

  def table9_entry(*v)
    index = @table9.length
    @table9 << v
    index
  end

  def write_table9
    o = write_table(@table9, length_prefix = false) do |s, entry|
      val1, val2, val3 = entry
      s.write([val1, val2, val3].pack('S<S<S<'))
    end
    at(TABLE9_OFFSET_LOCATION) { @data.write([o].pack('L<'))}
  end

  def write_script(script_data, entry_point, dialogue_line_count)
    pos = @data.pos
    at(SCRIPT_OFFSET_LOCATION) { @data.write([pos].pack('L<'))}
    at(DIALOGUE_LINE_COUNT_LOCATION) { @data.write([dialogue_line_count].pack('L<'))}
    @data.write([SCRIPT_MAGIC, entry_point].pack('L<L<'))
    @data.write(script_data)
    align_to(0xf)
    @data.write("\x00" * 16)
  end

  def write_table(table, length_prefix = true)
    offset = @data.pos
    result = StringIO.new
    result.seek(length_prefix ? 4 : 0)
    result.write([table.length].pack('L<'))
    table.each do |entry|
      yield result, entry
    end

    if length_prefix
      result.seek(0)
      result.write([result.length - 4].pack('L<'))
    end

    @data.write(result.string)
    align_to(0x3)

    offset
  end

  def self.to_shift_jis(str, halfwidth_replace = false)
    str = str.tr(HALFWIDTH_REPLACE, HALFWIDTH) if halfwidth_replace
    converted = CONVERTER.convert(str) + "\x00"
    # Replace fullwidth spaces to use Entergram's special one-byte space character (0xA0)
    converted = converted.gsub("\x81\x40".force_encoding('SHIFT_JIS'), "\xA0".force_encoding('SHIFT_JIS')) if halfwidth_replace
    converted
  end

  def align_to(align)
    @data.seek((@data.pos + align) & ~align)
  end

  def at(offset)
    cur = @data.pos
    @data.seek(offset)
    yield
    @data.seek(cur)
  end

  def current_offset
    @data.pos
  end
end

Register = Struct.new(:id)
Parameter = Struct.new(:value)

Raw = Struct.new(:str)

def raw_pack(val, paradigm); Raw.new([val].pack(paradigm)); end
def byte(val); raw_pack(val, 'C'); end
def short(val); raw_pack(val, 's<'); end
def ushort(val); raw_pack(val, 'S<'); end
def int(val); raw_pack(val, 'l<'); end
def uint(val); raw_pack(val, 'L<'); end

class KalScript
  def initialize(offset)
    @offset = offset
    @data = StringIO.new
    @data.binmode
    @labels = {}
    @label_fix = []

    @dialogue_line_count = 0
  end

  attr_reader :dialogue_line_count

  def lc
    int(@dialogue_line_count)
  end

  def data
    fix_labels
    @data.string
  end

  def label(name)
    raise "Label can not be :null" if name == :null
    val = @data.pos + @offset
    @labels[name] = val
    val
  end

  def go_absolute(target)
    @data.seek(target - @offset)
  end

  def ins(opcode, *data)
    @dialogue_line_count += 1 if opcode == 0x86
    @data.write([opcode].pack('C'))
    data.each do |e|
      if e.is_a? Raw
        @data.write(e.str)
      elsif e.is_a? String
        @data.write_str(e, opcode == 0x86)
      elsif e.is_a? Array
        @data.write([e.length].pack('C'))
        e.each { |f| write_varlen(f) }
      else
        write_varlen(e)
      end
    end
  end

  def write_varlen(e)
    if e.is_a? Integer
      write_varlen_const(e)
    elsif e.is_a? Register
      reg = e.id
      if reg >= 0x10
        @data.write([0xc0, reg].pack('CC'))
      else
        @data.write([0xb0 + reg].pack('C'))
      end
    elsif e.is_a? Parameter
      wrote = @data.write([0xd0 + e.value].pack('C'))
    elsif e == :null
      @data.write("\xe0")
    elsif e.is_a? Symbol # label
      @label_fix << [@data.pos, e]
      @data.write("\x00" * 4)
    else
      raise "Invalid varlen: #{e}"
    end
  end

  def write_varlen_const(e)
    if e <= 0x3f && e >= -0x40
      if e >= 0
        @data.write([e].pack('C'))
      else
        @data.write([e + 0x80].pack('C'))
      end
    elsif e <= 0x7ff && e >= -0x800
      if e >= 0
        @data.write([0x80 | ((e & 0x700) >> 8), e & 0xff].pack('CC'))
      else
        shifted = e + 0xfff
        @data.write([0x80 | ((shifted & 0xf00) >> 8), shifted & 0xff].pack('CC'))
      end
    elsif e <= 0x7ffff && e >= -0x80000
      if e >= 0
        @data.write([0x90 | ((e & 0x70000) >> 16), ((e & 0xff00) >> 8), e & 0xff].pack('CCC'))
      else
        shifted = e + 0xfffff
        @data.write([0x90 | ((shifted & 0xf0000) >> 16), ((shifted & 0xff00) >> 8), shifted & 0xff].pack('CCC'))
      end
    else
      raise "Varlen const #{e} too big or too small"
    end
  end

  def fix_labels
    cur = @data.pos
    @label_fix.each do |pos, name|
      @data.seek(pos)
      label_pos = @labels[name]
      raise "Reference to undefined address #{name}" if label_pos.nil?
      @data.write([label_pos].pack('L<'))
    end
    @data.seek(cur)
  end
end
