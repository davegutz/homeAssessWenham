# filter_csv.py  Selectively hand-fix format of csv from Assessors
# Copyright (C) 2024 Dave Gutz
#
# This library is free software; you can redistribute it and/or
# modify it under the terms of the GNU Lesser General Public
# License as published by the Free Software Foundation;
# version 2.1 of the License.
#
# This library is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU
# Lesser General Public License for more details.
#
# See http://www.fsf.org/licensing/licenses/lgpl.txt for full license text.

"""Process Assessor's file line by line to fix formatting issues such as commas embedded in field
of the csv data file.
"""

import numpy as np
import matplotlib.pyplot as plt
# below suppresses runtime error display******************
# import os
# os.environ["KIVY_NO_CONSOLELOG"] = "1"
# from kivy.utils import platform  # failed experiment to run BLE data plotting realtime on android
# if platform != 'linux':
#     from unite_pictures import unite_pictures_into_pdf, cleanup_fig_files
from Colors import Colors
import re
from local_paths import version_from_data_file, local_paths
import os
import sys
if sys.platform == 'darwin':
    import matplotlib
    matplotlib.use('tkagg')
plt.rcParams.update({'figure.max_open_warning': 0})


def write_clean_file(path_to_data=None, working_file=None, hdr_key=None):
    """First line with hdr_key defines the number of fields to be imported cleanly"""
    import os
    (path, basename) = os.path.split(path_to_data)
    csv_file = working_file

    # Header
    have_header_str = None
    num_fields = 0
    with open(path_to_data, "r", encoding='cp437') as input_file:
        with open(csv_file, "w") as output:
            try:
                for line in input_file:
                    if line.__contains__(hdr_key):
                        if have_header_str is None:
                            have_header_str = True  # write one title only
                            output.write(line)
                            num_fields = line.count(',')  # first line with hdr_key defines number of fields
            except IOError:
                print("filter_csv.py:", line)  # last line

    # Data
    num_lines = 0
    num_lines_in = 0
    num_skips = 0
    length = 0
    unit_key_found = False
    with (open(path_to_data, "r", encoding='cp437') as input_file):  # reads all characters even bad ones
        with open(csv_file, "a") as output:
            for line in input_file:
                if line.__contains__(',') and not line.__contains__('Config:'):
                    unit_key_found = True
                    if line.count(",") == num_fields and line.count(";") == 0 and \
                            re.search(r'[^a-zA-Z0-9+-_.:, ]', line[:-1]) is None:
                        output.write(line)
                        num_lines += 1

                    else:
                        print('discarding: ', line)
                        num_skips += 1
                    num_lines_in += 1
    if not num_lines:
        csv_file = None
        print("I(write_clean_file): no data to write")
        if not unit_key_found:
            print("W(write_clean_file):  unit_key not found in ", basename, ".  Looking with '{:s}'".format(unit_key))
    else:
        print("Wrote(write_clean_file):", csv_file, num_lines, "lines", num_skips, "skips", length, "fields")
    return csv_file


def main():
    data_file = './FY24FORDAVIDGUTZ.csv'
    working_file = './working_file.csv'
    write_clean_file(path_to_data=data_file, working_file=working_file, hdr_key='Wenham')


# import cProfile
# if __name__ == '__main__':
#     cProfile.run('main()')
#


if __name__ == '__main__':
    main()
