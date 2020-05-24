# -*- coding: utf-8 -*-
"""
Created on Mon May 11 08:28:59 2020

@author: Lucas J. Hovestadt
@desc: Data visualization for B.Sc. thesis
"""
#Import statements --------------------------------------------------------
#Data manipulation & storage
import pandas as pd
import numpy as np
#Data visualization
import plotly.graph_objects as go
from plotly.subplots import make_subplots
from plotly.offline import plot
#--------------------------------------------------------------------------

# Loading dataframe
path = './RDStoCSVfiles/toPyCompatible.csv'
df = pd.read_csv(path)

# Printing columns for easy reference
print(df.columns)

# Subsetting results
subset = df[np.logical_and((df['r2'] == .3), (df['n'] == 500))]

# Extracting columns
imputations = subset['imp']
miPrb = subset['mi.prb.x']
midPrb = subset['mid.prb.x']
ldPrb = subset['ld.prb.x']
cx = subset['cx']
ap = subset['ap']
pm = subset['pm']

#--------------------------------------------------------------------------

#Initializing subplots in a grid
fig = make_subplots(
    rows=1, cols=3,
    specs=[
            
           [{'type': 'scene'}, {'type': 'scene'}, {'type': 'scene'}]      

           ],
    subplot_titles=("Traditional Multiple Imputation (MI)",
                    "Multiple Imputation then Deletion (MID)",
                    "Listwise Deletion (LD)"),
    x_title='Scroll to zoom in, and out',
    y_title='Click and drag to rotate figures',
    )

# Adding the plots
fig.add_trace(go.Scatter3d(x=imputations, y=miPrb, z=cx, name="(MI)",
                           mode='markers', marker=dict(
        size=8,
        color=miPrb,             
        colorscale='Plasma',
        opacity=0.7
    )), row=1, col=1)

fig.add_trace(go.Scatter3d(x=imputations, y=midPrb, z=cx, name="(MID)",
                           mode='markers', marker=dict(
        size=8,
        color=midPrb,
        colorscale='Plasma',   
        opacity=0.7
    )), row=1, col=2)

fig.add_trace(go.Scatter3d(x=imputations, y=ldPrb, z=cx, name="(LD)",
                           mode='markers', marker=dict(
        size=8,
        color=ldPrb,             
        colorscale='Plasma',  
        opacity=0.7
    )), row=1, col=3)

#--------------------------------------------------------------------------

# Set font
fig.update_layout(
    font=dict(
        family="Courier New, monospace",
        size=10,
    )
)
    
# Set title
fig.update_layout(title_text='3D Scatterplot comparison of MI, MID, and LD (B.Sc. Thesis L.J. Hovestadt)', title_x=0.5, title_y=.95)
    
# Hide legend
fig.update_layout(showlegend=False)

# Set marker size
fig.update_traces(marker=dict(size=6))

# Initialize zoom and camera variable
zoom = 3
camera = dict(
    eye=dict(x=zoom, y=zoom - 1, z=zoom)
)

# Update initial camera view in figure
fig.update_scenes(camera=camera)

# Also update across all scenes
fig.update_scenes(
                  xaxis = dict(title_text="Imputations"),
                  yaxis = dict(title_text="PRB"),
                  zaxis = dict(title_text="Corr. x & z"))

#--------------------------------------------------------------------------

# Save figure in html format
#
# Anytime an adjustment is made this line has to be run again
# This is to regenerate the .html file.
plot(fig, filename='mi-mid-ld-comparison.html')



