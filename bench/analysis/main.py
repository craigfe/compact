#!/usr/bin/env python3

import sys
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns

df = pd.read_csv(sys.argv[1])

# Categorise the entries by implementation
# implementations=['stdlib','base','containers','backtracking','compact','compact-immediate']
# df['implementation'] = pd.Categorical(df['implementation'], implementations)

fig, (ax1, ax2, ax3) = plt.subplots(1, 3, figsize=(13, 5))

sns.lineplot(data = df, ax = ax1, x = 'entries', y = 'reachable_words', hue = 'implementation')
sns.lineplot(data = df, ax = ax2, x = 'entries', y = 'allocated_words', hue = 'implementation')
sns.boxplot(data = df, ax = ax3, x = 'implementation', y = 'time(ns)')

ax3.set_yscale('log')
ax1.grid(which ='major', color='gray', linewidth=0.2)
ax2.grid(which ='major', color='gray', linewidth=0.2)
ax3.grid(which ='minor', color='gray', linewidth=0.2)

fig.suptitle('Relative performance of hashset implementations')
fig.tight_layout()

figure_file = 'hashset_memory_usage.png'
print('Saving to \'%s\'' % figure_file)
fig.savefig(figure_file)
