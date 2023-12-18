import numpy as np
import pandas as pd
import matplotlib.pyplot as plt
import seaborn as sns
import matplotlib.pyplot as plt
import numpy as np
import matplotlib.pyplot as plt
import numpy as np
correlation_PL = pd.read_csv('.../Dataset_Figure4_Changes in within-group consistency.csv')
correlation_PL.head()

axesSub = sns.lineplot(x="Time(ms)", y="Representational Similarity Values (r) between Subjects", data=correlation_PL, color="#097CFF")
axesSub = sns.lineplot(x="Time(ms)", y="OT", data=correlation_PL, color="red", ax=axesSub)

plt.show()