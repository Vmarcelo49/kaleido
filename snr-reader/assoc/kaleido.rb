# Defines associations to make the output file more readable.
ADDRESSES = {
  # Labels
  'addr_0x8c19' => 'l_scenario_start',


  # Functions (directly called)
  'addr_0x4dd2' => 'f_ins_0x85_related_0x4dd2',

  'addr_0x4e37' => 'f_perform_transition',
  'addr_0x4e52' => 'f_perform_transition_no_default',

  # This seems to show a background under conditions.
  'addr_0x4f2b' => 'f_show_background_0x4f2b',
  'addr_0x4f39' => 'l_f_show_background_0x4f2b_px0_not_null',
  'addr_0x4f47' => 'l_f_show_background_0x4f2b_px1_not_null',

  'addr_0x54d3' => 'f_bg_related_0x54d3',

  'addr_0x674e' => 'f_sprite_related_0x674e',

  'addr_0x6eca' => 'f_sound_related_0x6eca',

  # This function updates rx19 based on a complex lookup table. If it is
  # successful, rx19 is set to px1
  'addr_0x6f98' => 'f_battler_0x6f98',
  'addr_0x6ff5' => 'l_f_battler_p0_3',
  'addr_0x701a' => 'l_f_battler_p0_5',
  'addr_0x7024' => 'l_f_battler_p0_30',
  'addr_0x7045' => 'l_f_battler_p0_31_32_33',
  'addr_0x706e' => 'l_f_battler_p0_34',
  'addr_0x7078' => 'l_f_battler_p0_35',
  'addr_0x7082' => 'l_f_battler_p0_36',
  'addr_0x70b4' => 'l_f_battler_update_rx19',
  'addr_0x70b4' => 'l_f_battler_return',

  'addr_0x7279' => 'f_lookup_table_0x7279',
  'addr_0x78ae' => 'l_f_lookup_table_0x7279_return',


  # Subroutines (gosub)
}

REGISTERS = {
  0x17 => '%bg_current',
  0x3f => '%rx3f_transition_related',
  0x40 => '%default_transition_duration',
}

# Which labels should be added in addition to the dynamically generated ones
REQUIRE_LABELS = Set.new([0x8c19])
