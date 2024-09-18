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


def write_clean_file_FY24FORDAVIDGUTZ(path_to_data=None, hdr_key=None, data_key=None,
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
                        hdr_fields = line.strip().split(',')
                        for hdr in hdr_fields:
                            if hdr != aux_hdr_key:
                                output.write(hdr + ';')
                        num_fields = line.count(';') - 1  # first line with hdr_key defines number of fields
                        output.write('\n')
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
                    if line.count(";") == num_fields + 1:
                        output.write(line.replace(addr_key, '').replace(',', '').replace('#', '').\
                                     replace(' ST;', ' STREET;').replace(' DR;', ' DRIVE;').
                                     replace(' AVE;', ' AVENUE;').replace(' RD;', ' ROAD;').
                                     replace(' CT;', ' COURT;').replace(' CIR;', ' CIRCLE;').
                                     replace(' LN;', ' LANE;').replace(' RD;', ' ROAD;').
                                     replace('BRUCE LN;', 'BRUCE LANE;').\
                                     replace('ARBOR STREET;', 'ARBOR ST;').\
                                     replace('BURLEY STREET;', 'BURLEY ST;').\
                                     replace('CHERRY STREET;', 'CHERRY ST;').\
                                     replace('ESSEX STREET;', 'ESSEX ST;').\
                                     replace('GRAPEVINE ROAD;', 'GRAPEVINE RD;').\
                                     replace('HULL STREET;', 'HULL ST;').\
                                     replace('MAIN STREET;', 'MAIN ST;').\
                                     replace('MAPLE STREET;', 'MAPLE ST;').\
                                     replace('PARSONS HILL ROAD;', 'PARSONS HILL RD;').\
                                     replace('PINE HILL ROAD;', 'PINE HILL RD;').\
                                     replace('PRINCEMERE LANE;', 'PRINCEMERE LN;').\
                                     replace('WALNUT ROAD;', 'WALNUT RD;').\
                                     replace('WM FAIRFIELD DRIVE', 'WM FAIRFIELD DR').\
                                     replace('\n', ";\n"))
                        num_lines += 1
                    else:
                        print('aux discarding: ', line, end='')
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
                        print('discarding: ', line, end='')
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
    # built_field = header_fields.index('Built')
    price_field = header_fields.index('Built')
    # date_field = header_fields.index('Date')
    # area_field = header_fields.index('Area')
    # value_field = header_fields.index('Value')

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
    for ll in range(num_price):
        price += int(field[inp_date_field - ll - 1]) * multiplier
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
        num_area = 1
        num_value = 2
    elif area_value.__len__() == 4:
        num_area = 2
        num_value = 2
    elif area_value.__len__() == 5:
        num_area = 2
        num_value = 3
    inp_value_start = inp_area_start + num_area

    area = 0
    multiplier = 1
    for i in range(num_area-1, -1, -1):
        area += multiplier * int(field[inp_area_start + i])
        multiplier *= 1000
    out += str(area) + ';'
    j += 1

    value = 0
    multiplier = 1
    for i in range(num_value-1, -1, -1):
        value += multiplier * int(field[inp_value_start + i])
        multiplier *= 1000
    out += str(value) + ';'
    j += 1

    # % Change field
    out += field[inp_entry_field - 1] + ';'
    j += 1

    # Check result ('Entry' was dropped)
    if j == (m - 1):
        return out + '\n'
    else:
        return None


def write_amended_file(blob, blob_aux, final_file):
    """ writes _clean_amend"""
    with open(final_file, "w") as out:
        nm = blob.dtype.names
        for i in range(nm.__len__()):
            out.write('"' + nm[i] + '",')
        out.write('"Type",' + '"Nhood",' + '\n')
        count = 0
        aux_len = blob_aux.__len__()
        print('aux len', aux_len)
        for i in range(blob.__len__()):
            for j in range(aux_len):
                if blob[i]['Location'] == blob_aux[j]['Address']:  # take first match (I know, could be erroneous)
                    for k in range(blob[i].__len__()):
                        out.write('"' + str(blob[i][k]) + '",')
                    count += 1
                    nhood = blob_aux[j]['Nhood']
                    if nhood == 'TB  ' or nhood == 'WP  ' or nhood == 'FC  ':
                        nhood = 0
                    out.write('"' + blob_aux[j]['Type'] + '","' + str(nhood) + '",\n')
            if j > blob_aux.__len__() - 1:
                print(blob[i]['Location'], " not found")
        print(f"{count=}")


def main():
    data_file = './FY24FORDAVIDGUTZ.csv'
    aux_file = './FY24REPORTFORDAVIDGUTZ.csv'
    final_file = './wenham_fy24.csv'

    data_file_clean, data_aux_file_clean = \
        write_clean_file_FY24FORDAVIDGUTZ(path_to_data=data_file, hdr_key='Header', data_key='Entry',
                                         path_to_aux=aux_file, aux_hdr_key='Land Area;Building Value', addr_key=' -   - WENHAM, MA 01984  ')
    blob = np.genfromtxt(data_file_clean, delimiter=';', encoding='utf-8', dtype=None, names=True)
    blob_aux = np.genfromtxt(data_aux_file_clean, delimiter=';', encoding='utf-8', dtype=None, names=True)
    missing = 0
    for i in range(blob_aux.__len__()):
        contains = blob['Location'].__contains__(blob_aux['Address'][i])
        if not contains:
            print(blob_aux['Address'][i])
            missing += 1
    print(f"Main length {blob.__len__()} Aux length {blob_aux.__len__()} missing {missing}")
    write_amended_file(blob, blob_aux, final_file)  # writes _clean_amend
# import cProfile
# if __name__ == '__main__':
#     cProfile.run('main()')
#


if __name__ == '__main__':
    main()
