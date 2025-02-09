import torch
import torch.nn as nn
import torch.optim as optim
import pytorch_lightning as pl
from torch.utils.data import DataLoader, TensorDataset, random_split

# ---- Autoencoder Definition ----
class Autoencoder(nn.Module):
    def __init__(self, input_dim=50, latent_dim=8):
        super(Autoencoder, self).__init__()
        
        # Encoder
        self.encoder = nn.Sequential(
            nn.Linear(input_dim, 32),
            nn.Tanh(),
            nn.Linear(32, 16),
            nn.Tanh(),
            nn.Linear(16, latent_dim),
        )
        
        # Decoder
        self.decoder = nn.Sequential(
            nn.Linear(latent_dim, 16),
            nn.Tanh(),
            nn.Linear(16, 32),
            nn.Tanh(),
            nn.Linear(32, input_dim),
        )

    def forward(self, x):
        z = self.encoder(x)
        x_recon = self.decoder(z)
        return x_recon

# ---- PyTorch Lightning Module ----
class LitAutoencoder(pl.LightningModule):
    def __init__(self, input_dim=50, latent_dim=8, lr=1e-3):
        super().__init__()
        self.autoencoder = Autoencoder(input_dim, latent_dim)
        self.criterion = nn.MSELoss()  # Reconstruction loss
        self.lr = lr

    def forward(self, x):
        return self.autoencoder(x)

    def training_step(self, batch, batch_idx):
        x, _ = batch  # Ignore labels if present
        x_recon = self.autoencoder(x)
        loss = self.criterion(x_recon, x)
        self.log("train_loss", loss, on_step=True, on_epoch=True, prog_bar=True)
        return loss

    def configure_optimizers(self):
        return optim.Adam(self.autoencoder.parameters(), lr=self.lr)

# ----  Dataset for Training ----
def get_dataloader(train_df, test_maj_df, test_min_dfs, device="cpu", batch_size=32):
     # for the train dataframe
    train_mean = train_df.mean(axis=0)
    train_sd = train_df.std(axis=0)
    z_train = torch.tensor(
        train_df.apply(lambda x: (x - train_mean) / train_sd, axis = 1).to_numpy(),
        dtype=torch.float32,
        device=device
    )

    # for the test dataframe
    #test_maj_mean = test_maj_df.mean(axis=0)
    #test_maj_sd = test_maj_df.std(axis=0)
    z_test_maj = torch.tensor(
        test_maj_df.apply(lambda x: (x - train_mean) / train_sd, axis = 1).to_numpy(),
        dtype=torch.float32,
        device=device
    )
    # for the test dataframe
    #test_min_mean = test_min_dfs.mean(axis=0)
    #test_min_sd = test_min_dfs.std(axis=0)
    z_test_min = torch.tensor(
        test_min_dfs.apply(lambda x: (x - train_mean) / train_sd, axis = 1).to_numpy(),
        dtype=torch.float32,
        device=device
    )

    train_dataset = TensorDataset(z_train, z_train)
    test_maj_dataset = TensorDataset(z_test_maj, z_test_maj)
    test_min_dataset = TensorDataset(z_test_min, z_test_min)

    train_loader = DataLoader(train_dataset, batch_size=batch_size, shuffle=True)
    test_maj_loader = DataLoader(test_maj_dataset, batch_size=batch_size, shuffle=False)
    test_min_loader = DataLoader(test_min_dataset, batch_size=batch_size, shuffle=False)
    return train_loader, test_maj_loader, test_min_loader