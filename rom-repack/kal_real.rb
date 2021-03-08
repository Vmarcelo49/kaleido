# Does the same thing as repack.rb, but allows replacing the SNR file with one
# compiled from a specified ruby script.

load './scenario.rb'
load './toolkit.rb'

snr = KalSNRFile.new

# Arguments:
# 1. ruby script to compile to SNR (e.g. minimal_example.rb in this folder)
# 2. source folder for assets to be packed into the rom file
# 3. location where the rom file will be written to
# 4. optional location where the SNR file will be written to by itself
raw, source_folder, target, snr_target = ARGV

load raw
raw_apply(snr)
snr.write_to(snr_target) if snr_target

f = KalRom2File.new

replacements = {
  'main.snr' => snr.data
}

f.files = load_packed_file_from_folder_recursive(source_folder, :root, replacements).content
f.write(target)
