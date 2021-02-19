# Defines associations to make the output file more readable.
ADDRESSES = {
  # Labels
  'addr_0x8c19' => 'l_scenario_start',

  # Functions (directly called)
  'addr_0x4f2b' => 'f_bg_related_0x4f2b',
  'addr_0x6eca' => 'f_sound_related_0x6eca',
  'addr_0x54d3' => 'f_bg_related_0x54d3',
  'addr_0x674e' => 'f_sprite_related_0x674e',
  'addr_0x4dd2' => 'f_ins_0x85_related_0x4dd2',


  # Subroutines (gosub)
}

# Which labels should be added in addition to the dynamically generated ones
REQUIRE_LABELS = Set.new([0x8c19])
