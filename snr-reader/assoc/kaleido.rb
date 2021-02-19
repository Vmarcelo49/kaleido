# Defines associations to make the output file more readable.
ADDRESSES = {
  # Labels
  'addr_0x8c19' => 'l_scenario_start',


  # Functions (directly called)
  'addr_0x4dd2' => 'f_ins_0x85_related_0x4dd2',

  'addr_0x4f2b' => 'f_bg_related_0x4f2b',
  'addr_0x4f39' => 'l_f_bg_related_0x4f2b_px0_not_null',
  'addr_0x4f47' => 'l_f_bg_related_0x4f2b_px1_not_null',

  'addr_0x54d3' => 'f_bg_related_0x54d3',

  'addr_0x674e' => 'f_sprite_related_0x674e',

  'addr_0x6eca' => 'f_sound_related_0x6eca',

  'addr_0x6f98' => 'f_calc_ins_0x51_related_0x6f98',

  'addr_0x7279' => 'f_lookup_table_0x7279',
  'addr_0x78ae' => 'l_f_lookup_table_0x7279_return',


  # Subroutines (gosub)
}

# Which labels should be added in addition to the dynamically generated ones
REQUIRE_LABELS = Set.new([0x8c19])
