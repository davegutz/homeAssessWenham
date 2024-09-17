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
# import matplotlib.pyplot as plt
# below suppresses runtime error display******************
# import os
# os.environ["KIVY_NO_CONSOLELOG"] = "1"
# from kivy.utils import platform  # failed experiment to run BLE data plotting realtime on android
# if platform != 'linux':
#     from unite_pictures import unite_pictures_into_pdf, cleanup_fig_files
# from Colors import Colors
import re
# from local_paths import version_from_data_file, local_paths
import os
import sys
if sys.platform == 'darwin':
    import matplotlib
    matplotlib.use('tkagg')


def write_clean_fil_FY24FORDAVIDGUTZ(path_to_data=None, hdr_key=None, data_key=None,
                                     path_to_aux=None, aux_hdr_key=None, addr_key=None):
    """First line with hdr_key defines the number of fields to be imported cleanly"""
    (path, base_file) = os.path.split(path_to_data)
    basename = base_file.split('.')[0]
    csv_file = basename + '_clean.csv'
    csv_aux_file = basename + '_aux_clean.csv'

    # aux file Header
    num_fields = 0
    with open(path_to_aux, "r", encoding='cp437') as input_file:
        with open(csv_aux_file, "w") as output:
            try:
                for line in input_file:
                    if line.__contains__(aux_hdr_key):
                        hdr_fields = line.split(',')
                        for hdr in hdr_fields:
                            if hdr != aux_hdr_key:
                                output.write(hdr + ';')
                        num_fields = line.count(',') - 1  # first line with hdr_key defines number of fields
                        break
            except IOError:
                print("filter_csv.py:", line)  # last line

    # aux file
    num_lines = 0
    num_skips = 0
    with (open(path_to_aux, "r", encoding='cp437') as input_file):  # reads all characters even bad ones
        with open(csv_aux_file, "a") as output:
            for line in input_file:
                if line.__contains__(addr_key):
                    if line.count(";") == num_fields and \
                            re.search(r'[^a-zA-Z0-9+-_.:, ]', line[:-1]) is None:
                        output.write(line.replace(addr_key, ''))
                        num_lines += 1
                    else:
                        print('discarding: ', line)
                        num_skips += 1
        if not num_lines:
            print("I(write_clean_file): no data to write")
        else:
            print("Wrote(write_clean_aux_file):", csv_aux_file, num_lines, "lines")

    # Header
    num_fields = 0
    with open(path_to_data, "r", encoding='cp437') as input_file:
        with open(csv_file, "w") as output:
            try:
                for line in input_file:
                    if line.__contains__(hdr_key):
                        hdr_fields = line.strip().split(',')
                        for hdr in hdr_fields:
                            if hdr != hdr_key and hdr != '':
                                num_fields += 1
                                print('hdr=', hdr + ';')
                                output.write(hdr + ';')
                        output.write("\n")
                        break
            except IOError:
                print("filter_csv.py:", line)  # last line

    # Data
    num_lines = 0
    num_skips = 0
    with (open(path_to_data, "r", encoding='cp437') as input_file):  # reads all characters even bad ones
        with open(csv_file, "a") as output:
            for line in input_file:
                if line.__contains__(',') and line.__contains__(data_key):
                    clean_line = mash_FY24FORDAVIDGUTZ(line)
                    if clean_line is not None:
                        output.write(clean_line)
                        num_lines += 1
                    else:
                        print('discarding: ', line)
                        num_skips += 1
    if not num_lines:
        csv_file = None
        print("I(write_clean_file): no data to write")
    else:
        print("Wrote(write_clean_file):", csv_file, num_lines, "lines", num_skips, "skips", num_fields, "fields")
    return csv_file, csv_aux_file


grade_dict = {'AA': 10,
              'A': 9,
              'A-': 8,
              'B+': 7,
              'B': 6,
              'B-': 5,
              'C+': 4,
              'C': 3,
              'C-': 2,
              'D+': 1,
              'D': 0,
              }


def mash_FY24FORDAVIDGUTZ(inp):
    """Crude mashup hand fix function"""
    header = 'ParcelID,Location,RM,BD,BA,3/4BA,HB,FP,Owned,NBC,Imp,Floor,Loc,Grade,Built,Price,Date,Area,Value,% Change,End of Report,'
    header_fields = header.split(',')
    grade_field = header_fields.index('Grade')
    built_field = header_fields.index('Built')
    price_field = header_fields.index('Built')
    date_field = header_fields.index('Date')
    area_field = header_fields.index('Area')
    value_field = header_fields.index('Value')

    # Fields before price
    inp = re.sub(' +', ' ', inp)  # remove extra spaces
    inp = re.sub(' ,', ',', inp)  # remove extra space before comma
    i = 0  # input index
    n = inp.count(',')
    out = ''
    j = 0  # output index
    m = header.count(',')
    field = inp.split(',')
    field[grade_field] = grade_dict.get(field[grade_field], -999)
    while i <= price_field and i < n:
        out += str(field[i]) + ';'
        i += 1
        j += 1

    # Price
    num_price = 0
    inp_date_field = i
    # find date field; it may be blank
    while not field[inp_date_field].count('/') == 2 and field[inp_date_field] != '':
        num_price += 1
        inp_date_field = i + num_price
    price = 0
    multiplier = 1
    for l in range(num_price):
        price += int(field[inp_date_field - l - 1]) * multiplier
        multiplier *= 1000
    out += str(price) + ';'
    j += 1
    i += num_price + 1

    # Date field
    out += field[inp_date_field] + ';'
    j += 1
    i += 1

    # Area field

    # Value field
    inp_entry_field = i
    num_entry = 0
    inp_area_start = inp_date_field + 1
    num_area = 1
    inp_value_start = None
    num_value = 0
    # find Entry field; it may be blank
    while not field[inp_entry_field] == 'Entry' and inp_entry_field < n:
        num_entry += 1
        inp_entry_field = i + num_entry
    area_value = field[inp_date_field+1:inp_entry_field-1]
    if area_value.__len__() == 2:
        num_area = 1
        num_value = 1
    elif area_value.__len__() == 3:
        num_area = 2
        num_value = 1
    elif area_value.__len__() == 4:
        num_area = 2
        num_value = 2
    elif area_value.__len__() == 3:
        num_area = 3
        num_value = 2
    inp_value_start = inp_area_start + num_area + 1
    out += field[inp_area_start + num_area -1] + ';'
    j += 1
    out += field[inp_value_start + num_value - 1] + ';'
    j += 1

    # % Change field
    out += field[inp_entry_field - 1] + ';'
    j += 1

    # Check result ('Entry' was dropped)
    if j == (m - 1):
        return out + '\n'
    else:
        return None


def main():
    data_file = './FY24FORDAVIDGUTZ.csv'
    aux_file = './FY24REPORTFORDAVIDGUTZ.csv'
    data_file_clean, data_aux_file_clean = \
        write_clean_fil_FY24FORDAVIDGUTZ(path_to_data=data_file, hdr_key='Header', data_key='Entry',
                                         path_to_aux=aux_file, aux_hdr_key='Land Area;Building Value', addr_key=' -   - WENHAM, MA 01984  ')
    # blob = np.genfromtxt(data_file_clean, delimiter=',', names=True).view(np.recarray)
    # blob_aux = np.genfromtxt(data_aux_file_clean, delimiter=';', names=True).view(np.recarray)

# import cProfile
# if __name__ == '__main__':
#     cProfile.run('main()')
#


if __name__ == '__main__':
    main()
