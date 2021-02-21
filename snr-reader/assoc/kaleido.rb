# Defines associations to make the output file more readable.
ADDRESSES = {
  # Labels
  'addr_0x8c19' => 'l_scenario_start',

  # Functions (directly called)
  'addr_0x4c6d' => 'f_init_0x4c6d',
  'addr_0x4d07' => 'f_init_current_sprite_values',
  'addr_0x4d68' => 'f_init_graphics',
  'addr_0x4dab' => 'f_reset_0x4dab',

  'addr_0x4dd2' => 'f_ins_0x85_related_0x4dd2',

  'addr_0x4e37' => 'f_perform_transition',
  'addr_0x4e52' => 'f_perform_transition_no_default',

  # This seems to show a background under conditions.
  'addr_0x4f2b' => 'f_show_background_0x4f2b',
  'addr_0x4f39' => 'l_f_show_background_0x4f2b_px0_not_null',
  'addr_0x4f47' => 'l_f_show_background_0x4f2b_px1_not_null',

  'addr_0x50dc' => 'f_set_sprite_transform',

  'addr_0x54d3' => 'f_bg_related_0x54d3',

  'addr_0x6c47' => 'f_hide_all_sprites',
  'addr_0x6c60' => 'f_hide_all_sprites_loop',
  'addr_0x6c89' => 'f_hide_all_sprites_done',

  # Loads a sprite and displays it.
  # Parameters:
  # %px0 = character number
  # %px1 is some sort of bit flag: &1 => bupload, &2 => faceload
  # %px2 unknown
  # %px3 = ID of face
  # %px4 = ?
  # %px5 = z index (0 => 1900, 1 => 1800, 2 => 1700, 3 => 1600)
  # %px6 = some flag (if > 0, some stuff is skipped)
  'addr_0x674e' => 'f_show_bustup_sprite',
  'addr_0x6774' => 'l_f_show_bustup_sprite_px2_not_null',
  'addr_0x6796' => 'l_f_show_bustup_sprite_px3_not_null',
  'addr_0x67b8' => 'l_f_show_bustup_sprite_px4_not_null',
  'addr_0x67da' => 'l_f_show_bustup_sprite_px5_not_null',
  'addr_0x6846' => 'l_f_show_bustup_sprite_save',
  'addr_0x685d' => 'l_f_show_bustup_sprite_start_bupload',
  'addr_0x6894' => 'l_f_show_bustup_sprite_px6_is_0',
  'addr_0x690e' => 'l_f_show_bustup_sprite_start_z',
  'addr_0x69de' => 'l_f_show_bustup_sprite_start_faceload',
  'addr_0x6a58' => 'l_f_show_bustup_sprite_start_bupclear',
  'addr_0x6a94' => 'l_f_show_bustup_sprite_start_faceclear',
  'addr_0x6ae3' => 'l_f_show_bustup_sprite_save_return',
  'addr_0x6b2f' => 'l_f_show_bustup_sprite_no_save_return',

  'addr_0x6e87' => 'f_set_sprite_properties_0x6e87',

  'addr_0x6eca' => 'f_sound_related_0x6eca',

  # This function updates rx19 based on a complex lookup table. If it is
  # successful, rx19 is set to px1
  'addr_0x6f98' => 'f_lookup_bg_related_battler',
  'addr_0x6ff5' => 'l_f_lookup_battler_p0_3',
  'addr_0x701a' => 'l_f_lookup_battler_p0_5',
  'addr_0x7024' => 'l_f_lookup_battler_p0_30',
  'addr_0x7045' => 'l_f_lookup_battler_p0_31_32_33',
  'addr_0x706e' => 'l_f_lookup_battler_p0_34',
  'addr_0x7078' => 'l_f_lookup_battler_p0_35',
  'addr_0x7082' => 'l_f_lookup_battler_p0_36',
  'addr_0x70b4' => 'l_f_lookup_battler_update_rx19',
  'addr_0x70b4' => 'l_f_lookup_battler_return',

  # Another lookup table, updates rx1e
  # %px0 = character number
  # %px1 = %px2 of show_bustup_sprite
  # %px2 = face id
  'addr_0x70b5' => 'f_lookup_sprite_related_keiichi',
  'addr_0x70dd' => 'l_f_lookup_keiichi_karin_doremi',
  'addr_0x7133' => 'l_f_lookup_keiichi_mina',
  'addr_0x717f' => 'l_f_lookup_keiichi_fuuka_char4',
  'addr_0x71dd' => 'l_f_lookup_keiichi_return',

  # Sets %rx1f to the first sprite in the given group
  # %px0 = character number, %px1 = %rx1e
  'addr_0x71ea' => 'f_lookup_first_sprite_index_by_char',
  'addr_0x720c' => 'l_f_lookup_first_sprite_index_karin',
  'addr_0x7227' => 'l_f_lookup_first_sprite_index_mina',
  'addr_0x7242' => 'l_f_lookup_first_sprite_index_doremi',
  'addr_0x725d' => 'l_f_lookup_first_sprite_index_fuuka_char4',
  'addr_0x7278' => 'l_f_lookup_first_sprite_index_return',


  'addr_0x7279' => 'f_lookup_table_0x7279',
  'addr_0x78ae' => 'l_f_lookup_table_0x7279_return',


  # Subroutines (gosub)
}

# Characters:
# 0 = Fuuka
# 1 = Karin
# 2 = Mina
# 3 = Doremi

REGISTERS = {
  # 0x11: always either 0 or 1
  # 0x13: always either 0 or 1

  0x17 => '%bg_current',


  0x1f => '%index_of_first_sprite_in_group',

  # &1 => bupload
  # &2 => faceload

  0x20 => '%current_sprite_mode',

  0x21 => '%fuuka_current_sprite_rx20',
  0x22 => '%karin_current_sprite_rx20',
  0x23 => '%mina_current_sprite_rx20',
  0x24 => '%doremi_current_sprite_rx20',
  0x25 => '%char4_current_sprite_rx20',

  0x26 => '%fuuka_current_sprite_px2',
  0x27 => '%karin_current_sprite_px2',
  0x28 => '%mina_current_sprite_px2',
  0x29 => '%doremi_current_sprite_px2',
  0x2a => '%char4_current_sprite_px2',

  0x2b => '%fuuka_current_face',
  0x2c => '%karin_current_face',
  0x2d => '%mina_current_face',
  0x2e => '%doremi_current_face',
  0x2f => '%char4_current_face',

  0x30 => '%fuuka_current_sprite_px4',
  0x31 => '%karin_current_sprite_px4',
  0x32 => '%mina_current_sprite_px4',
  0x33 => '%doremi_current_sprite_px4',
  0x34 => '%char4_current_sprite_px4',

  0x35 => '%fuuka_current_sprite_px5',
  0x36 => '%karin_current_sprite_px5',
  0x37 => '%mina_current_sprite_px5',
  0x38 => '%doremi_current_sprite_px5',
  0x39 => '%char4_current_sprite_px5',

  0x3a => '%fuuka_relative_z',
  0x3b => '%karin_relative_z',
  0x3c => '%mina_relative_z',
  0x3d => '%doremi_relative_z',
  0x3e => '%char4_relative_z',

  0x3f => '%rx3f_transition_related',
  0x40 => '%default_transition_duration',
}

FF_CALLS = {
  "char:%d bupload(%d,%d)" => "ff_bupload",
  "char:%d bupclear" => "ff_bupclear",
}

SPRITE_SLOT_MAIN = -6


# Which labels should be added in addition to the dynamically generated ones
REQUIRE_LABELS = Set.new([0x8c19])
