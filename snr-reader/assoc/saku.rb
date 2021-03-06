MODE = :saku

# Defines associations to make the output file more readable.
ADDRESSES = {
  'addr_0x932df' => 'l_scenario_start', # Start of Episode 1

  'addr_0x92d69' => 'f_hide_all_sprites',

  # %px0: slot
  # %px1: sprite id
  # %px2: x position?
  # %px3: y position?
  'addr_0x92d9a' => 'f_show_sprite',

  'addr_0x92dcd' => 'f_show_background',
}

REGISTERS = {}

FF_CALLS = {}

SPRITE_SLOT_MAIN = -6 # ?


# Which labels should be added in addition to the dynamically generated ones
REQUIRE_LABELS = Set.new
