#!/usr/bin/env python3

import argparse
import csv
import os
import re


def parse_args():
    parser = argparse.ArgumentParser()

    parser.add_argument("-d", "--datadir", required=True)
    parser.add_argument("-o", "--outfile", required=True)

    return parser.parse_args()


def extract_throughputs(filename):
    elapsed_re = re.compile(r"^elapsed:\s+([0-9\.]+)", re.MULTILINE)
    transactions_re = re.compile(r"^committed:\s+([0-9\.]+)", re.MULTILINE)

    throughputs = []

    with open(filename, "r") as f:
        filetext = f.read()
        transactions_matches = transactions_re.findall(filetext)
        elapsed_matches = elapsed_re.findall(filetext)

        assert(len(transactions_matches) == len(elapsed_matches))

        transactions_matches = list(
            map(lambda m: int(m), transactions_matches))
        elapsed_matches = list(map(lambda m: float(m), elapsed_matches))

        if len(transactions_matches) == 4:
            # Remove init and warmup numbers
            del transactions_matches[1:3]
            del elapsed_matches[1:3]
        elif len(transactions_matches) != 1 and len(transactions_matches) != 2:
            raise ValueError(
                "Length of transaction_matches expected to be 1, 2, or 4")

        # transactions_matches[0] below because hard to infer # transactions from
        # backup commits due to variable-size transactions
        for i in range(len(transactions_matches)):
            throughputs.append(transactions_matches[0] / elapsed_matches[i])

    return throughputs


def process_throughputs(datadir, outfile):

    cc_re = re.compile(r"alg@([^_]+)")
    i_re = re.compile(r"seq@([^_]+)")
    ccc_re = re.compile(r"ccc@(\S+?)__")
    n_warehouses_re = re.compile(r"warehouse_count@([^_]+)")
    n_districts_re = re.compile(r"district_count@([^_]+)")
    n_customers_re = re.compile(r"customer_count@([^_]+)")
    n_threads_re = re.compile(r"thread_count@([^_]+)")
    n_workers_re = re.compile(r"worker_count@([^_]+)")

    with open(outfile, 'w+') as f:
        writer = csv.writer(f)
        headers = ["cc", "ccc", "server", "i", "n_warehouses", "n_districts",
                   "n_customers", "n_clients", "n_workers", "throughput_tps", ]
        writer.writerow(headers)

        for entry in os.scandir(datadir):
            if entry.is_file() and not re.match(r".*\.failed-*", entry.path) and not re.match(r".*\.csv", entry.path) and not re.match(r".*\.pdf", entry.path):
                print(entry.path)
                cc = cc_re.search(entry.path).group(1)
                ccc = ccc_re.search(entry.path).group(1)
                i = i_re.search(entry.path).group(1)
                n_warehouses = n_warehouses_re.search(entry.path).group(1)
                n_districts = n_districts_re.search(entry.path).group(1)
                n_customers = n_customers_re.search(entry.path).group(1)
                n_threads = n_threads_re.search(entry.path).group(1)
                n_workers = n_workers_re.search(entry.path).group(1)

                tputs = extract_throughputs(entry.path)

                server = "primary"
                for tput in tputs:
                    row = [cc, ccc, server, i, n_warehouses, n_districts,
                           n_customers, n_threads, n_workers, tput]
                    writer.writerow(map(lambda x: str(x), row))
                    server = "backup"


def main():
    args = parse_args()

    datadir = args.datadir
    outfile = args.outfile

    process_throughputs(datadir, outfile)


if __name__ == "__main__":
    main()
