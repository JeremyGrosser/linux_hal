/*
 * Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
#include <linux/spi/spidev.h>
#include <sys/ioctl.h>
#include <stdint.h>

int linux_spi_ioc_message(int fd, uint8_t len, struct spi_ioc_transfer *msg) {
    return ioctl(fd, SPI_IOC_MESSAGE(len), msg);
}

int linux_spi_set_max_speed(int fd, uint32_t *hz) {
    return ioctl(fd, SPI_IOC_WR_MAX_SPEED_HZ, hz);
}

int linux_spi_get_max_speed(int fd, uint32_t *hz) {
    return ioctl(fd, SPI_IOC_RD_MAX_SPEED_HZ, hz);
}
