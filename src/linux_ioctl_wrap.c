/*
 * Copyright (C) 2023 Jeremy Grosser <jeremy@synack.me>
 *
 * SPDX-License-Identifier: BSD-3-Clause
 */
#include <linux/spi/spidev.h>
#include <linux/i2c-dev.h>
#include <linux/i2c.h>
#include <sys/ioctl.h>
#include <stdint.h>

int linux_spi_ioc_message(int fd, uint8_t len, struct spi_ioc_transfer *msg) {
    return ioctl(fd, SPI_IOC_MESSAGE(len), msg);
}

int linux_spi_set_max_speed(int fd, uint32_t hz) {
    return ioctl(fd, SPI_IOC_WR_MAX_SPEED_HZ, &hz);
}

int linux_spi_get_max_speed(int fd, uint32_t *hz) {
    return ioctl(fd, SPI_IOC_RD_MAX_SPEED_HZ, hz);
}

int linux_i2c_set_slave_address(int fd, int addr) {
    return ioctl(fd, I2C_SLAVE, addr);
}

int linux_i2c_set_tenbit_addressing(int fd, int enabled) {
    return ioctl(fd, I2C_TENBIT, (long)enabled);
}

int linux_i2c_set_pec(int fd, int enabled) {
    return ioctl(fd, I2C_PEC, (long)enabled);
}

void linux_i2c_set_timeout(int fd, int ms) {
    ioctl(fd, I2C_TIMEOUT, ms / 10);
    /*
     * I2C_TIMEOUT will only fail if fd is closed or ms exceeds INT_MAX, both
     * of which are prevented by Ada's assertions beforehand. Therefore, we can
     * ignore the return value of this function.
     */
}
