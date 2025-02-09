# This script runs the autoencoder described in hte "Feature selection for imnbalanced data with deep sparse autoencoders ensemble"

# Import relevant libraries
import pandas as pd
from sparse_autoencoder.Autoencoder import LitAutoencoder, get_dataloader
import torch
import torch.nn as nn
import pytorch_lightning as pl
from pytorch_lightning.callbacks import ModelCheckpoint
from sklearn.metrics import r2_score
import numpy as np
import random
import os
import yaml


device = torch.device("mps") if torch.backends.mps.is_available() else "cpu"

# Set the seed
def seed_everything(seed=42):
    random.seed(seed)
    os.environ["PYTHONHASHSEED"] = str(seed)
    np.random.seed(seed)
    torch.manual_seed(seed)
    torch.backends.cudnn.deterministic = True
    torch.backends.cudnn.benchmark = False

# Read the config of the experiment
with open("dsae_config.yaml") as stream:
    try:
        config = yaml.safe_load(stream)
    except yaml.YAMLError as exc:
        print(exc)

# Import data frame
df = pd.read_csv("train_test_val_data/train_data.csv")
df.head()

# Divide dataset into bacteremic and non-bacteremic patients
df_bact = df.loc[df["bacteremia"] == 1].reset_index(drop=True)
# print(df_bact.shape)
df_bact.head()
df_no_bact = df.loc[df["bacteremia"] == 0].reset_index(drop=True)
# print(df_no_bact.shape)

df_no_bact.head()
B = config["B"]
O = df_bact.shape[0]
test_maj_dfs = {}
test_min_dfs = {}
train_dfs = {}
for b in range(B):
    majority_test = df_no_bact.sample(O, random_state=b)
    test_maj_dfs[b] = majority_test.reset_index(drop=True)
    test_min_dfs[b] = df_bact.reset_index(drop=True)
    train_dfs[b] = (
        pd.concat([majority_test, df_no_bact], axis=0)
        .drop_duplicates(keep=False)
        .reset_index(drop=True)
    )

temp_train_df = train_dfs[0].drop(columns=["sex", "id", "bacteremia"])
temp_test_maj_df = test_maj_dfs[0].drop(columns=["sex", "id", "bacteremia"])
temp_test_min_df = test_min_dfs[0].drop(columns=["sex", "id", "bacteremia"])

# Constructing the two RE matrices, I think I will skip the step of making Q as just one and directly calculate the minority class one and
# the majority class one
RE_minority = []
RE_majority = []
batch_size = 32
n_epochs = config["n_epochs"]
save_every_n_epochs = config["save_every_n_epochs"]
latent_dim = config["latent_dim"]
r2 = {"test_maj": [], "test_min": [], "train": []}
seed_everything(42)
EXP_PATH = f"experiment_dim_{latent_dim}_B_{B}"
load_model_epoch = config["load_model_epoch"] 
if load_model_epoch is not None:
    load_model_epoch -= 1

os.makedirs(EXP_PATH, exist_ok=load_model_epoch is not None)

for b in range(B):
    temp_train_df = train_dfs[b].drop(columns=["sex", "id", "bacteremia"])
    temp_test_maj_df = test_maj_dfs[b].drop(columns=["sex", "id", "bacteremia"])
    temp_test_min_df = test_min_dfs[b].drop(columns=["sex", "id", "bacteremia"])
    train_data_loader, test_maj_loader, test_min_loader = get_dataloader(
        temp_train_df, temp_test_maj_df, temp_test_min_df, device=device, batch_size=batch_size
    )

    # Train model (or load it)
    model = LitAutoencoder(input_dim=temp_train_df.shape[1], latent_dim=latent_dim).to(device)
    if load_model_epoch is None:
        checkpoint_callback = ModelCheckpoint(
            filename="autoencoder_{epoch}",
            every_n_epochs=save_every_n_epochs,
            save_top_k=-1
        )
        trainer = pl.Trainer(
            callbacks=[checkpoint_callback],
            default_root_dir=EXP_PATH+f"/model_{b}",
            max_epochs=n_epochs
        )
        trainer.fit(model, train_data_loader)
    else:
        LOAD_PATH = EXP_PATH + f"/model_{b}/lightning_logs/version_0/checkpoints/autoencoder_epoch={load_model_epoch}.ckpt"
        model = LitAutoencoder.load_from_checkpoint(LOAD_PATH, input_dim=temp_train_df.shape[1], latent_dim=latent_dim)

    model.eval()
    model.to(device)

    targets_test_min = torch.empty(
        size=(temp_test_min_df.shape[0], temp_test_min_df.shape[1])
    )
    preds_test_min = torch.empty(
        size=(temp_test_min_df.shape[0], temp_test_min_df.shape[1])
    )

    targets_test_maj = torch.empty(
        size=(temp_test_maj_df.shape[0], temp_test_maj_df.shape[1])
    )
    preds_test_maj = torch.empty(
        size=(temp_test_maj_df.shape[0], temp_test_maj_df.shape[1])
    )

    targets_train = torch.empty(size=(temp_train_df.shape[0], temp_train_df.shape[1]))
    preds_train = torch.empty(size=(temp_train_df.shape[0], temp_train_df.shape[1]))

    with torch.no_grad():
        for batch_idx, batch in enumerate(test_min_loader):
            X, _ = batch
            curr_pred = model(X)
            begin_idx = batch_idx * batch_size
            end_idx = begin_idx + X.shape[0]
            targets_test_min[begin_idx:end_idx, :] = X
            preds_test_min[begin_idx:end_idx, :] = curr_pred
        for batch_idx, batch in enumerate(test_maj_loader):
            X, _ = batch
            curr_pred = model(X)
            begin_idx = batch_idx * batch_size
            end_idx = begin_idx + X.shape[0]
            targets_test_maj[begin_idx:end_idx, :] = X
            preds_test_maj[begin_idx:end_idx, :] = curr_pred
        for batch_idx, batch in enumerate(train_data_loader):
            X, _ = batch
            curr_pred = model(X)
            begin_idx = batch_idx * batch_size
            end_idx = begin_idx + X.shape[0]
            targets_train[begin_idx:end_idx, :] = X
            preds_train[begin_idx:end_idx, :] = curr_pred

    r2["test_maj"].append(r2_score(y_true=targets_test_maj, y_pred=preds_test_maj))
    r2["test_min"].append(r2_score(y_true=targets_test_min, y_pred=preds_test_min))
    r2["train"].append(r2_score(y_true=targets_train, y_pred=preds_train))

    criterion = nn.MSELoss(reduction="none")
    RE_test_min = criterion(preds_test_min, targets_test_min)
    RE_test_maj = criterion(preds_test_maj, targets_test_maj)

    # Append RE_test_min as a row in RE_minority
    RE_minority.append(RE_test_min)
    RE_majority.append(RE_test_maj)

# Convert to tensor if needed
RE_minority = torch.cat(RE_minority, dim=0)
RE_majority = torch.cat(RE_majority, dim=0)

n_epochs = n_epochs if load_model_epoch is None else (load_model_epoch + 1)
DATA_SAVE_PATH = EXP_PATH + f"/n_epochs_{n_epochs}_"

pd.DataFrame(r2).to_csv(
    DATA_SAVE_PATH + "r2.csv", index=False
)

pd.DataFrame(RE_minority.numpy(), columns=temp_train_df.columns).to_csv(
    DATA_SAVE_PATH + "RE_minority.csv", index=False
)
pd.DataFrame(RE_majority.numpy(), columns=temp_train_df.columns).to_csv(
    DATA_SAVE_PATH + "RE_majority.csv", index=False
)
