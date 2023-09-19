import asyncio
import serial_asyncio
import queue
import datetime

port = 'COM6'
filename = '2023-03-16_gems_test_all_mass.csv'

incoming_serial_queue = queue.Queue()

def parse_gems(b):
    str_rn = b.decode()
    str = str_rn.rstrip()
    str_c = str.removeprefix('R:')
    str_c = str_c.removesuffix(',\x00')
    str_d = str_c.split(',')
    dat = [float(str) for str in str_d]
    return dat

async def read_serial(port, queue):
    print("do serial")
    reader, writer = await serial_asyncio.open_serial_connection(url = port, baudrate = 9600)
    while True:
        raw = await reader.readline()
        data = parse_gems(raw)
        ts = datetime.datetime.now()
        queue.put((ts, data))

async def main():
    print("main")
    with open(filename, 'a', buffering = 1) as the_file:
        #the_file.write("timestamp,power,temp,pressure,co2\n")
        while True:
            if not incoming_serial_queue.empty():
                data = incoming_serial_queue.get()
                data_str = ','.join(map(str,data[1]))
                print("get: " + str(data[0]) + ',' + data_str)
                the_file.write(str(data[0]) + ',' + data_str + '\n')
            await asyncio.sleep(0.01)

async def start():
    print("start")
    await asyncio.gather(
        read_serial(port, incoming_serial_queue),
        main(),
    )

if __name__ == '__main__':
    asyncio.run(start())


