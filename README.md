# Reverse engineering Gensou Rougoku no Kaleidoscope

This is just something I am doing for fun and to get a bit more familiar with reverse engineering and VN scripting. Perhaps eventually a PC port of the VN will come out of it.

The bulk of this project is in the `read_scenario.rb` file that parses the `main.snr` file from the Switch release. Extracting and converting the other files is possible e.g. with tools from 07th Mod.

I have not yet tried whether this parsing system will work with Umineko Saku. It will most certainly not work out of the box because Umineko Saku will have a lot of features that Kaleido doesn't have, but it remains to be seen whether Umineko Saku does anything completely differently. In any case, the insights gained from decompiling Kaleido will be useful in future Entergram decompilation projects.

Current status:

 - [x] Successfully parses the entire file
 - [x] Dialogue
 - [ ] Backgrounds
 - [ ] Sprites
 - [ ] BGM
 - [ ] SE
 - [ ] Voices
 - [ ] Choices
 - [ ] Movies
 - [ ] Menu
 - [ ] Loading, saving
 - [ ] Bonus content
